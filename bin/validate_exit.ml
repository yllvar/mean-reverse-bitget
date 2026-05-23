open Bitget_lib

let fee = 0.001
let initial_cash = 1000.0

type portfolio = {
  cash : float;
  position : float;
  entry_price : float;
  trades : int;
  wins : int;
  total_won : float;
  total_lost : float;
  equity_peak : float;
}

let init_pf = {
  cash = initial_cash; position = 0.0; entry_price = 0.0;
  trades = 0; wins = 0;
  total_won = 0.0; total_lost = 0.0;
  equity_peak = initial_cash;
}

let equity pf price = pf.cash +. pf.position *. price

let close_position pf price =
  let revenue = pf.position *. price *. (1.0 -. fee) in
  let pnl = revenue -. (pf.entry_price *. pf.position) in
  if pnl > 0.0 then
    { pf with cash = pf.cash +. revenue; position = 0.0; entry_price = 0.0;
      trades = pf.trades + 1; wins = pf.wins + 1;
      total_won = pf.total_won +. pnl }
  else
    { pf with cash = pf.cash +. revenue; position = 0.0; entry_price = 0.0;
      trades = pf.trades + 1;
      total_lost = pf.total_lost +. (-. pnl) }

type exit_mode = SignalOnly | SlTpOnly | SignalSlTp

let run_backtest prices ~window_size ~threshold ~ema_period ~exit_mode ~stop_loss ~take_profit ~position_size =
  let n = Array.length prices in
  let min_len = window_size + (if ema_period > 0 then ema_period else 0) + 1 in
  if n < min_len then None
  else
    let window = Array.make window_size 0.0 in
    for i = 0 to window_size - 1 do
      window.(i) <- prices.(i)
    done;
    let ema_win =
      let arr = Array.make ema_period 0.0 in
      for i = 0 to ema_period - 1 do
        arr.(i) <- prices.(i)
      done;
      arr
    in
    let pf = ref init_pf in
    let max_dd = ref 0.0 in
    let w_idx = ref 0 in
    let e_idx = ref 0 in
    let w_sum = ref (Array.fold_left (+.) 0.0 window) in
    let w_sq_sum = ref (Array.fold_left (fun a x -> a +. x *. x) 0.0 window) in
    let e_sum = ref (Array.fold_left (+.) 0.0 ema_win) in

    for i = min_len to n - 1 do
      let price = prices.(i) in
      let mean = !w_sum /. float window_size in
      let variance = !w_sq_sum /. float window_size -. mean *. mean in
      let stddev = if variance > 0.0 then sqrt variance else 0.0 in
      let raw_signal =
        if stddev < 0.01 then Strategy.Hold
        else
          let z = (price -. mean) /. stddev in
          if z < -.threshold then Strategy.Buy
          else if z > threshold then Strategy.Sell
          else Strategy.Hold
      in
      let ema = !e_sum /. float ema_period in
      let signal = Strategy.apply_trend_filter raw_signal price ema in

      if !pf.position > 0.0 then (
        let sl_price = !pf.entry_price *. (1.0 -. stop_loss) in
        let tp_price = !pf.entry_price *. (1.0 +. take_profit) in
        let should_exit =
          match exit_mode with
          | SignalOnly -> signal = Strategy.Sell
          | SlTpOnly -> price <= sl_price || price >= tp_price
          | SignalSlTp -> signal = Strategy.Sell || price <= sl_price || price >= tp_price
        in
        if should_exit then pf := close_position !pf price
      ) else (
        if signal = Strategy.Buy then
          let invest = !pf.cash *. position_size in
          let qty = invest *. (1.0 -. fee) /. price in
          pf := { !pf with cash = !pf.cash -. invest; position = qty; entry_price = price }
        else if signal = Strategy.Sell then
          ()
      );

      let eq = equity !pf price in
      let peak = max !pf.equity_peak eq in
      pf := { !pf with equity_peak = peak };
      let dd = if peak > 0.0 then (peak -. eq) /. peak *. 100.0 else 0.0 in
      if dd > !max_dd then max_dd := dd;

      let old_w = window.(!w_idx) in
      window.(!w_idx) <- price;
      w_sum := !w_sum -. old_w +. price;
      w_sq_sum := !w_sq_sum -. old_w *. old_w +. price *. price;
      w_idx := (!w_idx + 1) mod window_size;

      let old_e = ema_win.(!e_idx) in
      ema_win.(!e_idx) <- price;
      e_sum := !e_sum -. old_e +. price;
      e_idx := (!e_idx + 1) mod ema_period
    done;

    let pf = !pf in
    let last_price = prices.(n - 1) in
    let final_eq = equity pf last_price in
    let total_return = (final_eq -. initial_cash) /. initial_cash *. 100.0 in
    let win_rate =
      if pf.trades = 0 then 0.0
      else float pf.wins /. float pf.trades *. 100.0
    in
    let profit_factor =
      if pf.total_lost = 0.0 then Float.infinity
      else pf.total_won /. pf.total_lost
    in
    Some (pf.trades, win_rate, profit_factor, !max_dd, total_return)

let load_csv path =
  let ic = open_in path in
  let buf = Buffer.create 1000000 in
  let rec loop () =
    try
      let line = input_line ic in
      match String.split_on_char '|' line with
      | _ts :: _o :: _h :: _l :: close :: _rest ->
        Buffer.add_string buf (close ^ "\n");
        loop ()
      | _ -> loop ()
    with End_of_file -> close_in ic
  in
  loop ();
  let content = Buffer.contents buf in
  Array.of_list (
    List.filter_map (fun s ->
      let s = String.trim s in
      if s = "" then None
      else try Some (float_of_string s) with Failure _ -> None
    ) (String.split_on_char '\n' content)
  )

let mode_name = function
  | SignalOnly -> "Signal-only"
  | SlTpOnly -> "SL/TP-only"
  | SignalSlTp -> "Signal+SL/TP"

let () =
  let csv_path =
    try Sys.argv.(1) with _ -> "data/SOLUSDT.csv"
  in
  let prices = load_csv csv_path in
  let n = Array.length prices in
  let min_p = Array.fold_left min Float.max_float prices in
  let max_p = Array.fold_left max Float.neg_infinity prices in
  Printf.eprintf "Loaded %d candles from %s (%.2f .. %.2f)\n%!" n csv_path min_p max_p;

  let window_size = 500 in
  let threshold = 1.0 in
  let ema_period = 200 in
  let position_size = 0.5 in

  Printf.printf "\n=== Exit Strategy Comparison ===\n%!";
  Printf.printf "Params: window=%d threshold=%.1f ema=%d pos_size=%.0f%%\n%!"
    window_size threshold ema_period (position_size *. 100.0);
  Printf.printf "\n%-15s %-7s %-7s %-7s %-8s  %s\n%!"
    "mode" "trades" "win%" "pf" "max_dd" "return";

  let modes = [
    (SignalOnly, 0.0, 0.0);
    (SlTpOnly, 0.30, 0.15);
    (SignalSlTp, 0.30, 0.15);
  ] in

  let best_ret = ref (-1e10) in
  let best_mode = ref "" in

  List.iter (fun (mode, sl, tp) ->
    match run_backtest prices ~window_size ~threshold ~ema_period ~exit_mode:mode
        ~stop_loss:sl ~take_profit:tp ~position_size with
    | Some (trades, wr, pf, mdd, ret) ->
      let pf_str = if pf = Float.infinity then "inf" else Printf.sprintf "%.2f" pf in
      Printf.printf "%-15s %-7d %-6.1f%% %-7s %-7.2f%%  %+.2f%%\n%!"
        (mode_name mode) trades wr pf_str mdd ret;
      if ret > !best_ret then begin
        best_ret := ret;
        best_mode := mode_name mode
      end
    | None -> ()
  ) modes;

  Printf.printf "\nBest: %s (%+.2f%%)\n%!" !best_mode !best_ret;

  Printf.printf "\n=== SL Grid Search (Signal+SL mode, no TP) ===\n%!";
  Printf.printf "Params: window=%d threshold=%.1f ema=%d (signal exits primary, SL crash protection only)\n%!"
    window_size threshold ema_period;
  Printf.printf "\n%-5s %-7s %-7s %-7s %-8s  %s\n%!"
    "SL" "trades" "win%" "pf" "max_dd" "return";

  let sl_values = [0.10; 0.20; 0.30; 0.40; 0.50; 1.0] in

  List.iter (fun sl ->
    match run_backtest prices ~window_size ~threshold ~ema_period ~exit_mode:SignalSlTp
        ~stop_loss:sl ~take_profit:100.0 ~position_size with
    | Some (trades, wr, pf, mdd, ret) ->
      let pf_str = if pf = Float.infinity then "inf" else Printf.sprintf "%.2f" pf in
      Printf.printf "%-4.0f%% %-7d %-6.1f%% %-7s %-7.2f%%  %+.2f%%\n%!"
        (sl *. 100.0) trades wr pf_str mdd ret
    | None -> ()
  ) sl_values
