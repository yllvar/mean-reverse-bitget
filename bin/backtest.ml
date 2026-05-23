open Bitget_lib

let fee = 0.001

type portfolio = {
  cash : float;
  position : float;
  entry_value : float;
  trades : int;
  wins : int;
  total_won : float;
  total_lost : float;
  equity_peak : float;
}

let init_pf = {
  cash = 1000.0; position = 0.0; entry_value = 0.0;
  trades = 0; wins = 0;
  total_won = 0.0; total_lost = 0.0;
  equity_peak = 1000.0;
}

let equity pf price = pf.cash +. pf.position *. price

let apply_trade pf signal price =
  match signal with
  | Strategy.Buy when pf.position = 0.0 ->
    let pos = pf.cash *. (1.0 -. fee) /. price in
    { pf with cash = 0.0; position = pos; entry_value = pf.cash }
  | Strategy.Sell when pf.position > 0.0 ->
    let revenue = pf.position *. price *. (1.0 -. fee) in
    let pnl = revenue -. pf.entry_value in
    if pnl > 0.0 then
      { pf with cash = revenue; position = 0.0;
        trades = pf.trades + 1; wins = pf.wins + 1;
        total_won = pf.total_won +. pnl }
    else
      { pf with cash = revenue; position = 0.0;
        trades = pf.trades + 1;
        total_lost = pf.total_lost +. (-. pnl) }
  | _ -> pf

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

let run_backtest prices window_size threshold sma_period =
  let n = Array.length prices in
  let min_len = window_size + (if sma_period > 0 then sma_period else 0) + 1 in
  if n < min_len then None
  else
    let window = Array.make window_size 0.0 in
    for i = 0 to window_size - 1 do
      window.(i) <- prices.(i)
    done;
    let sma_win =
      if sma_period > 0 then
        let arr = Array.make sma_period 0.0 in
        for i = 0 to sma_period - 1 do
          arr.(i) <- prices.(i)
        done;
        arr
      else [||]
    in
    let pf = ref init_pf in
    let max_dd = ref 0.0 in
    let w_idx = ref 0 in
    let s_idx = ref 0 in
    let w_sum = ref (Array.fold_left (+.) 0.0 window) in
    let w_sq_sum = ref (Array.fold_left (fun a x -> a +. x *. x) 0.0 window) in
    let s_sum = ref (if sma_period > 0 then Array.fold_left (+.) 0.0 sma_win else 0.0) in

    for i = min_len to n - 1 do
      let price = prices.(i) in
      let mean = !w_sum /. float window_size in
      let variance = !w_sq_sum /. float window_size -. mean *. mean in
      let stddev = if variance > 0.0 then sqrt variance else 0.0 in
      let signal =
        if stddev < 0.01 then Strategy.Hold
        else
          let z = (price -. mean) /. stddev in
          if z < -.threshold then Strategy.Buy
          else if z > threshold then Strategy.Sell
          else Strategy.Hold
      in
      let signal =
        if sma_period <= 0 then signal
        else
          let sma = !s_sum /. float sma_period in
          Strategy.apply_trend_filter signal price sma
      in
      pf := apply_trade !pf signal price;
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

      if sma_period > 0 then begin
        let old_s = sma_win.(!s_idx) in
        sma_win.(!s_idx) <- price;
        s_sum := !s_sum -. old_s +. price;
        s_idx := (!s_idx + 1) mod sma_period
      end
    done;

    let pf = !pf in
    let last_price = prices.(n - 1) in
    let final_eq = equity pf last_price in
    let total_return = (final_eq -. 1000.0) /. 1000.0 *. 100.0 in
    let win_rate =
      if pf.trades = 0 then 0.0
      else float pf.wins /. float pf.trades *. 100.0
    in
    let profit_factor =
      if pf.total_lost = 0.0 then Float.infinity
      else pf.total_won /. pf.total_lost
    in
    Some (pf.trades, win_rate, profit_factor, !max_dd, total_return)

let () =
  let csv_path =
    try Sys.argv.(1) with _ -> "data/SOLUSDT.csv"
  in
  let prices = load_csv csv_path in
  let n = Array.length prices in
  Printf.eprintf "Loaded %d candles from %s (%.2f .. %.2f)\n%!"
    n csv_path
    (Array.fold_left min Float.max_float prices)
    (Array.fold_left max Float.neg_infinity prices);

  let windows = [10; 20; 50; 100; 200] in
  let thresholds = [1.0; 1.5; 2.0; 2.5; 3.0] in
  let sma_periods = [0; 50; 100; 200; 500] in

  Printf.printf "%-7s %-5s %-4s %-6s %-6s %-7s %-9s  %s\n%!"
    "window" "sig" "sma" "trades" "win%" "pf" "max_dd" "return";

  let best_ret = ref (-1e10) in
  let best_label = ref "" in
  let best_trades = ref 0 in
  let best_wr = ref 0.0 in
  let best_pf = ref 0.0 in
  let best_mdd = ref 0.0 in

  List.iter (fun w ->
    List.iter (fun t ->
      List.iter (fun s ->
        match run_backtest prices w t s with
        | Some (trades, wr, pf, mdd, ret) ->
          Printf.printf "%-7d %-5.1f %-4d %-6d %-5.1f%% %-7.2f %-7.2f%% %+.2f%%\n%!"
            w t s trades wr pf mdd ret;
          if ret > !best_ret then begin
            best_ret := ret;
            best_label := Printf.sprintf "window=%d sigma=%.1f sma=%d" w t s;
            best_trades := trades;
            best_wr := wr;
            best_pf := pf;
            best_mdd := mdd
          end
        | None -> ()
      ) sma_periods
    ) thresholds
  ) windows;

  Printf.printf "\nBest: %s\n%!" !best_label;
  Printf.printf "  Trades: %d  Win%%: %.1f  PF: %.2f  MaxDD: %.2f%%  Return: %.2f%%\n%!"
    !best_trades !best_wr !best_pf !best_mdd !best_ret
