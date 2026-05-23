open Lwt.Infix

let fee = 0.001
let initial_cash = 1000.0

type state = {
  prices : float list;
  window_size : int;
  ema_prices : float list;
  ema_period : int;
  threshold : float;
  interval : float;
  cycle : int;
  virtual_cash : float;
  virtual_position : float;
  virtual_trades : int;
  has_position : bool;
  entry_price : float;
  position_qty : float;
  stop_loss : float;
  take_profit : float;
  position_size : float;
}

let init_state ~prices ~ema_prices ~window_size ~ema_period ~threshold ~interval
    ~stop_loss ~take_profit ~position_size =
  { prices; window_size; ema_prices; ema_period; threshold; interval; cycle = 0;
    virtual_cash = initial_cash; virtual_position = 0.0; virtual_trades = 0;
    has_position = false; entry_price = 0.0; position_qty = 0.0;
    stop_loss; take_profit; position_size }

let append_price state price =
  let prices = price :: state.prices in
  let prices =
    if List.length prices > state.window_size then
      List.rev (List.tl (List.rev prices))
    else prices
  in
  let ema_prices = price :: state.ema_prices in
  let ema_prices =
    if List.length ema_prices > state.ema_period then
      List.rev (List.tl (List.rev ema_prices))
    else ema_prices
  in
  { state with prices; ema_prices; cycle = state.cycle + 1 }

let format_ts () =
  let t = Unix.time () in
  let tm = Unix.localtime t in
  Printf.sprintf "%02d:%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

let signal_label = function
  | Strategy.Buy -> "BUY  ↓"
  | Strategy.Sell -> "SELL ↑"
  | Strategy.Hold -> "HOLD —"

let max_backoff = 60

let log_order_result json_str =
  try
    let code, msg, result = Types.parse_order_response json_str in
    if code = "00000" then begin
      Printf.eprintf "[ORDER] OK: id=%s client_id=%s\n%!" result.Types.order_id result.Types.client_order_id;
      Printf.printf "  Order OK: id=%s client_id=%s\n%!" result.Types.order_id result.Types.client_order_id
    end else begin
      Printf.eprintf "[ORDER] FAILED: code=%s msg=%s\n%!" code msg;
      Printf.printf "  Order FAILED: code=%s msg=%s\n%!" code msg
    end
  with exn ->
    Printf.eprintf "[ORDER] PARSE ERROR: %s\n%!" (Printexc.to_string exn);
    Printf.printf "  Order response: %s\n%!" json_str

let print_state state current_price z ema signal reason =
  let equity = state.virtual_cash +. state.virtual_position *. current_price in
  let pnl_pct = (equity -. initial_cash) /. initial_cash *. 100.0 in
  let ts = format_ts () in
  let pos_str =
    if state.has_position then
      Printf.sprintf "POS %.4f SOL @ %.2f" state.position_qty state.entry_price
    else "NO POS"
  in
  Printf.printf "[%s] #%-4d  Price: %8.2f  z: %+6.3f  EMA: %8.2f  Signal: %s  %s  P&L: $%.2f (%+.2f%%)  trades: %d\n%!"
    ts state.cycle current_price z ema (signal_label signal) pos_str equity pnl_pct state.virtual_trades;
  Printf.printf "SIGNAL,%d,%.2f,%+.3f,%s,%d\n%!"
    (int_of_float (Unix.time ())) current_price z
    (match signal with
      | Strategy.Buy -> "BUY" | Strategy.Sell -> "SELL" | Strategy.Hold -> "HOLD")
    (List.length state.prices);
  if reason <> "" then Printf.printf "  >>> %s\n%!" reason

let handle_buy ~api_key ~secret ~passphrase ~symbol state current_price z ema =
  let invest = state.virtual_cash *. state.position_size in
  let qty = invest *. (1.0 -. fee) /. current_price in
  let qty_str = string_of_float qty in
  Bitget.place_market_order ~api_key ~secret ~passphrase ~symbol
    ~side:"buy" ~quantity:qty_str >>= fun resp ->
  log_order_result resp;
  let state = { state with
    has_position = true; entry_price = current_price; position_qty = qty;
    virtual_cash = state.virtual_cash -. invest; virtual_position = qty;
  } in
  print_state state current_price z ema Strategy.Buy "BUY";
  Lwt.return (Ok state)

let handle_sell ~api_key ~secret ~passphrase ~symbol state current_price z ema =
  if state.has_position then
    let qty_str = string_of_float state.position_qty in
    Bitget.place_market_order ~api_key ~secret ~passphrase ~symbol
      ~side:"sell" ~quantity:qty_str >>= fun resp ->
    log_order_result resp;
    let state = { state with
      has_position = false; entry_price = 0.0; position_qty = 0.0;
      virtual_cash = state.virtual_cash +. state.position_qty *. current_price *. (1.0 -. fee);
      virtual_position = 0.0;
      virtual_trades = state.virtual_trades + 1;
    } in
    print_state state current_price z ema Strategy.Sell "SELL (close position)";
    Lwt.return (Ok state)
  else begin
    print_state state current_price z ema Strategy.Sell "SELL (no position)";
    Lwt.return (Ok state)
  end

let handle_hold state current_price z ema =
  print_state state current_price z ema Strategy.Hold "";
  Lwt.return (Ok state)

let handle_sl_tp ~api_key ~secret ~passphrase ~symbol state current_price z ema signal reason =
  let qty_str = string_of_float state.position_qty in
  Bitget.place_market_order ~api_key ~secret ~passphrase ~symbol
    ~side:"sell" ~quantity:qty_str >>= fun resp ->
  log_order_result resp;
  let state = { state with
    has_position = false; entry_price = 0.0; position_qty = 0.0;
    virtual_cash = state.virtual_cash +. state.position_qty *. current_price *. (1.0 -. fee);
    virtual_position = 0.0;
    virtual_trades = state.virtual_trades + 1;
  } in
  print_state state current_price z ema signal reason;
  Lwt.return (Ok state)

let process_ticker ~stop ~api_key ~secret ~passphrase ~symbol state =
  Bitget.ticker ~symbol >>= fun ticker_json ->
  (try
    match Types.parse_ticker_response ticker_json with
    | _, _, [t] ->
      let current_price = float_of_string t.last_pr in
      let state = append_price state current_price in
      let ema = Strategy.ema ~prices:state.ema_prices ~period:state.ema_period in
      let signal, z =
        Strategy.mean_reversion ~prices:state.prices ~current_price ~threshold:state.threshold
      in
      let filtered = Strategy.apply_trend_filter signal current_price ema in
      if state.has_position then
        let sl_price = state.entry_price *. (1.0 -. state.stop_loss) in
        let tp_price = state.entry_price *. (1.0 +. state.take_profit) in
        if current_price <= sl_price then
          handle_sl_tp ~api_key ~secret ~passphrase ~symbol state current_price z ema Strategy.Sell
            (Printf.sprintf "STOP LOSS @ %.2f (entry=%.2f)" current_price state.entry_price)
        else if current_price >= tp_price then
          handle_sl_tp ~api_key ~secret ~passphrase ~symbol state current_price z ema Strategy.Sell
            (Printf.sprintf "TAKE PROFIT @ %.2f (entry=%.2f)" current_price state.entry_price)
        else if filtered = Strategy.Sell then
          handle_sl_tp ~api_key ~secret ~passphrase ~symbol state current_price z ema Strategy.Sell
            (Printf.sprintf "SIGNAL EXIT @ %.2f z=%+.3f (entry=%.2f)" current_price z state.entry_price)
        else
          handle_hold state current_price z ema
      else
        begin match filtered with
        | Strategy.Buy -> handle_buy ~api_key ~secret ~passphrase ~symbol state current_price z ema
        | Strategy.Sell -> handle_sell ~api_key ~secret ~passphrase ~symbol state current_price z ema
        | Strategy.Hold -> handle_hold state current_price z ema
        end
    | code, msg, _ ->
      Printf.eprintf "Ticker error (code=%s msg=%s)\n%!" code msg;
      Lwt.return (Error "ticker")
   with exn ->
     Printf.eprintf "Parse error: %s\n%!" (Printexc.to_string exn);
     Lwt.return (Error "parse"))

let rec loop ~stop ~api_key ~secret ~passphrase ~symbol ~backoff state =
  if !stop then begin
    Printf.printf "\n=== Bot stopped after %d cycles ===\n%!" state.cycle;
    Lwt.return ()
  end else
    Lwt.catch (fun () -> process_ticker ~stop ~api_key ~secret ~passphrase ~symbol state)
      (fun exn ->
        Printf.eprintf "Network error: %s\n%!" (Printexc.to_string exn);
        Lwt.return (Error "network"))
    >>= function
    | Ok state ->
      Lwt_unix.sleep state.interval >>= fun () ->
      loop ~stop ~api_key ~secret ~passphrase ~symbol ~backoff:1 state
    | Error _ ->
      let delay = min backoff max_backoff in
      Printf.eprintf "Retrying in %ds...\n%!" delay;
      Lwt_unix.sleep (float delay) >>= fun () ->
      loop ~stop ~api_key ~secret ~passphrase ~symbol
        ~backoff:(min (backoff * 2) max_backoff) state

let run ~api_key ~secret ~passphrase ~symbol initial_state =
  let stop = ref false in
  let _ = Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ ->
    stop := true;
    print_endline ""
  )) in
  loop ~stop ~api_key ~secret ~passphrase ~symbol ~backoff:1 initial_state
