open Bitget_lib
open Lwt.Infix
open Strategy

let product_type = "USDT-FUTURES"
let granularity = "1H"
let ema_fast = 12
let ema_slow = 50
let total_capital = 1000.0
let allocation = total_capital /. 4.0
let fee = 0.0006
let symbols = ["SPYUSDT"; "SOXLUSDT"; "XAUUSDT"; "QQQUSDT"]

type inst = {
  symbol : string;
  prices : float list;
  pos : position_type;
  entry_price : float;
  qty : float;
  realized_pnl : float;
}

type order = {
  o_symbol : string;
  o_side : string;
  o_qty : float;
}

let symbol_label = function
  | "SPYUSDT"   -> "SPY"
  | "SOXLUSDT"  -> "SOXL"
  | "XAUUSDT"   -> "XAU"
  | "NDX100USDT" -> "NDX1"
  | s -> s

let load_env () =
  let chan = open_in ".env" in
  let finally () = close_in chan in
  Fun.protect ~finally (fun () ->
    try
      while true do
        let line = input_line chan in
        match String.split_on_char '=' line with
        | k :: v :: rest ->
          Unix.putenv k (String.concat "=" (v :: rest))
        | _ -> ()
      done
    with End_of_file -> ()
  )

let get_env_nonempty name =
  let v = try Unix.getenv name with Not_found -> "" in
  if v = "" then failwith (Printf.sprintf "%s not set or empty" name)
  else v

let rec take n acc = function
  | _ when n <= 0 -> List.rev acc
  | x :: xs -> take (n - 1) (x :: acc) xs
  | [] -> List.rev acc

let ema_value prices period =
  Strategy.compute_ema_value ~prices ~period

let avg_range highs lows =
  let n = List.length highs in
  if n < 5 then 0.0
  else
    let sum = List.fold_left2 (fun acc h l -> acc +. (h -. l)) 0.0 highs lows in
    sum /. float n

let format_ts () =
  let t = Unix.time () in
  let tm = Unix.localtime t in
  Printf.sprintf "%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min

let unrealized_pnl inst cur_price =
  match inst.pos with
  | Long  -> (cur_price -. inst.entry_price) *. inst.qty
  | Short -> (inst.entry_price -. cur_price) *. inst.qty
  | Flat  -> 0.0

let trade_cost inst entry exit =
  let revenue = inst.qty *. exit *. (1.0 -. fee) in
  let cost = inst.qty *. entry in
  match inst.pos with
  | Long  -> revenue -. cost
  | Short -> cost -. revenue
  | Flat  -> 0.0

let compute_qty price =
  let raw = allocation /. price in
  max 0.01 (floor (raw *. 100.0) /. 100.0)

let make_order_side = function
  | Flat, Buy  -> Some "buy"
  | Flat, Sell -> Some "sell"
  | Long, Sell -> Some "sell"
  | Long, Buy  -> None
  | Long, Hold -> None
  | Short, Buy  -> Some "buy"
  | Short, Sell -> None
  | Short, Hold -> None
  | Flat, Hold  -> None

let next_position = function
  | Flat, Buy  -> Long
  | Flat, Sell -> Short
  | Long, Sell -> Flat
  | Long, Buy  -> Long
  | Long, Hold -> Long
  | Short, Buy  -> Flat
  | Short, Sell -> Short
  | Short, Hold -> Short
  | Flat, Hold  -> Flat

let place_order ~api_key ~secret ~passphrase order =
  let qty_str = Printf.sprintf "%.2f" order.o_qty in
  Bitget.mix_market_order ~api_key ~secret ~passphrase
    ~symbol:order.o_symbol ~productType:product_type
    ~side:order.o_side ~quantity:qty_str >>= fun resp ->
  try
    let code, msg, result = Types.parse_order_response resp in
    if code = "00000" then begin
      Printf.eprintf "[ORDER OK] %s %s %s: id=%s\n%!"
        order.o_symbol order.o_side qty_str result.Types.order_id;
      Lwt.return true
    end else begin
      Printf.eprintf "[ORDER FAIL] %s %s %s: code=%s msg=%s\n%!"
        order.o_symbol order.o_side qty_str code msg;
      Lwt.return false
    end
  with exn ->
    Printf.eprintf "[ORDER ERR] %s %s %s: %s\n%!" order.o_symbol order.o_side qty_str
      (Printexc.to_string exn);
    Lwt.return false

let dry_run = ref false

let rec cycle ~stop ~api_key ~secret ~passphrase instruments first_iter =
  if !stop then begin
    Printf.printf "\n=== Stock Trader Stopped ===\n%!";
    Lwt.return ()
  end else
    Lwt.catch (fun () ->
      let fetch_one sym =
        Bitget.mix_candles ~symbol:sym ~productType:product_type
          ~granularity ~limit:ema_slow >>= fun json ->
        try
          let code, msg, candles = Types.parse_candle_response json in
          if code <> "00000" then begin
            Printf.eprintf "[%s] API error: code=%s msg=%s\n%!" sym code msg;
            Lwt.return_none
          end else begin
            let closes = List.filter_map (fun (c : Types.candle) ->
              try Some (float_of_string c.close) with _ -> None) candles in
            let opens = List.filter_map (fun (c : Types.candle) ->
              try Some (float_of_string c.open_p) with _ -> None) candles in
            let highs = List.filter_map (fun (c : Types.candle) ->
              try Some (float_of_string c.high) with _ -> None) candles in
            let lows = List.filter_map (fun (c : Types.candle) ->
              try Some (float_of_string c.low) with _ -> None) candles in
            let n = List.length closes in
            if n < ema_slow then begin
              Printf.eprintf "[%s] Not enough candles: %d\n%!" sym n;
              Lwt.return_none
            end else begin
              let prices = List.rev closes in
              let high50 = take ema_slow [] highs in
              let low50 = take ema_slow [] lows in
              Lwt.return_some (prices, List.hd closes, List.hd highs,
                               List.hd lows, List.hd opens, avg_range high50 low50)
            end
          end
        with exn ->
          Printf.eprintf "[%s] Parse error: %s\n%!" sym (Printexc.to_string exn);
          Lwt.return_none
      in
      Lwt_list.map_s fetch_one symbols >>= fun results ->
      let has_any = List.exists (function Some _ -> true | None -> false) results in
      if not has_any then begin
        Printf.eprintf "No data, sleeping 60s...\n%!";
        Lwt_unix.sleep 60.0 >>= fun () ->
        cycle ~stop ~api_key ~secret ~passphrase instruments first_iter
      end else begin
        let orders = ref [] in
        let new_insts = ref [] in
        let total_eq = ref 0.0 in
        List.iter2 (fun data_opt (inst : inst) ->
          match data_opt with
          | None ->
            new_insts := inst :: !new_insts
          | Some (prices, cur_close, cur_high, cur_low, cur_open, avg_rng) ->
            let fast = ema_value prices ema_fast in
            let slow = ema_value prices ema_slow in
            let signal = trend_following ~fast_ema:fast ~slow_ema:slow
                           ~current_price:cur_close in
            let new_inst =
              if first_iter then { inst with prices }
              else
                let action_signal =
                  match inst.pos with
                  | Flat ->
                    if signal = Buy || signal = Sell then Some signal else None
                  | Long | Short ->
                    trend_exit_signal ~position:inst.pos
                      ~fast_ema:fast ~slow_ema:slow
                      ~high:cur_high ~low:cur_low ~close:cur_close
                      ~open_:cur_open ~avg_range:avg_rng
                in
                let new_sig = match action_signal with Some s -> s | None -> signal in
                let order_side = make_order_side (inst.pos, new_sig) in
                let new_pos = next_position (inst.pos, new_sig) in
                let new_rp =
                  match order_side with
                  | Some _ when inst.pos <> Flat ->
                    inst.realized_pnl +. trade_cost inst inst.entry_price cur_close
                  | _ -> inst.realized_pnl
                in
                let new_qty = match order_side with
                  | Some _ -> compute_qty cur_close
                  | None -> inst.qty
                in
                let new_entry = match order_side with
                  | Some _ -> cur_close
                  | None -> inst.entry_price
                in
                begin match order_side with
                | Some side when not first_iter ->
                  orders := { o_symbol = inst.symbol; o_side = side; o_qty = new_qty } :: !orders
                | _ -> ()
                end;
                { symbol = inst.symbol; prices;
                  pos = new_pos; entry_price = new_entry; qty = new_qty;
                  realized_pnl = new_rp }
            in
            let upnl = unrealized_pnl new_inst cur_close in
            let eq = new_inst.realized_pnl +. upnl in
            total_eq := !total_eq +. eq;
            let sig_s = match signal with Buy -> "BUY" | Sell -> "SELL" | Hold -> "HOLD" in
            let pos_s = match new_inst.pos with
              | Long -> "LONG" | Short -> "SHORT" | Flat -> "FLAT" in
            let entry_s = if new_inst.pos = Flat then "—"
              else Printf.sprintf "%.2f" new_inst.entry_price in
            Printf.printf "%-5s %8.2f %8.2f %8.2f %-5s %-6s %9s %+8.2f\n%!"
              (symbol_label inst.symbol) cur_close fast slow sig_s pos_s entry_s eq;
            new_insts := new_inst :: !new_insts
        ) results instruments;
        let new_insts = List.rev !new_insts in
        let orders = List.rev !orders in
        Printf.printf "%s\n%!" (String.make 64 '-');
        Printf.printf "Total P&L: $%.2f\n%!" !total_eq;
        (match orders with
         | [] -> Lwt.return ()
         | _ when !dry_run ->
           Printf.eprintf "[DRY-RUN] Would place %d orders:\n%!" (List.length orders);
           List.iter (fun o ->
             Printf.eprintf "  %s %s %.4f\n%!" o.o_symbol o.o_side o.o_qty
           ) orders;
           Lwt.return ()
         | _ ->
           Printf.eprintf "Placing %d orders...\n%!" (List.length orders);
           Lwt_list.map_s (place_order ~api_key ~secret ~passphrase) orders >>= fun _ ->
           Lwt.return ()
        ) >>= fun () ->
        Lwt_unix.sleep 3600.0 >>= fun () ->
        cycle ~stop ~api_key ~secret ~passphrase new_insts false
      end
    ) (fun exn ->
      Printf.eprintf "Loop error: %s\n%!" (Printexc.to_string exn);
      Lwt_unix.sleep 60.0 >>= fun () ->
      cycle ~stop ~api_key ~secret ~passphrase instruments first_iter
    )

let init_state symbol =
  { symbol; prices = []; pos = Flat; entry_price = 0.0; qty = 0.0; realized_pnl = 0.0 }

let () =
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "--dry-run" then
    dry_run := true;
  (try load_env () with _ -> ());
  let api_key = get_env_nonempty "BITGET_API_KEY" in
  let secret = get_env_nonempty "BITGET_SECRET_KEY" in
  let passphrase = get_env_nonempty "BITGET_PASSPHRASE" in

  print_endline "=== Bitget Stock Perp Trend Follower ===";
  Printf.eprintf "Instruments: %s\n%!" (String.concat ", " symbols);
  Printf.eprintf "Strategy: %d-EMA / %d-EMA on %s candles\n%!" ema_fast ema_slow granularity;
  Printf.eprintf "Capital: $%.2f ($%.2f per instrument)\n%!" total_capital allocation;
  if !dry_run then print_endline ">>> DRY RUN MODE — no real orders <<<";

  let instruments = List.map init_state symbols in
  let stop = ref false in
  let _ = Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ ->
    stop := true; print_endline ""
  )) in
  Lwt_main.run (cycle ~stop ~api_key ~secret ~passphrase instruments true)
