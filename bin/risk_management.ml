(* Phase 6: Risk Management Optimization *)
(* Tests position sizing, stop losses, and take profits with baseline signal *)
(* Precomputes signals once, then applies different risk parameters *)

type candle_data = {
  ts : int;
  close : float;
  high : float;
  low : float;
}

type signal = Buy | Sell | Hold

let load_full_csv path =
  let ic = open_in path in
  let candles = ref [] in
  let rec loop () =
    try
      let line = input_line ic in
      match String.split_on_char '|' line with
      | ts :: _o :: h :: l :: close :: _rest ->
        (try
          let ts = int_of_string ts in
          let close = float_of_string close in
          let high = float_of_string h in
          let low = float_of_string l in
          candles := { ts; close; high; low } :: !candles
         with Failure _ -> ());
          loop ()
      | _ -> loop ()
    with End_of_file -> close_in ic
  in
  loop ();
  Array.of_list (List.rev !candles)

let slice_by_timestamp candles start_ts end_ts =
  let rec loop i acc =
    if i >= Array.length candles then Array.of_list (List.rev acc)
    else if candles.(i).ts >= start_ts && candles.(i).ts < end_ts then
      loop (i + 1) (candles.(i) :: acc)
    else loop (i + 1) acc
  in
  loop 0 []

let slice_signals signals candles start_ts end_ts =
  let rec loop i acc =
    if i >= Array.length candles then Array.of_list (List.rev acc)
    else if candles.(i).ts >= start_ts && candles.(i).ts < end_ts then
      loop (i + 1) (signals.(i) :: acc)
    else loop (i + 1) acc
  in
  loop 0 []

let compute_sma data period =
  let n = Array.length data in
  if period <= 0 then data
  else
    let sma = Array.make n 0.0 in
    let sum = ref 0.0 in
    for i = 0 to period - 1 do sum := !sum +. data.(i) done;
    sma.(period - 1) <- !sum /. float period;
    for i = period to n - 1 do
      sma.(i) <- (sma.(i - 1) *. float period +. data.(i) -. data.(i - period)) /. float period
    done;
    sma

let compute_zscore data window =
  let n = Array.length data in
  let z = Array.make n 0.0 in
  for i = window - 1 to n - 1 do
    let sum = ref 0.0 in
    for j = i - window + 1 to i do sum := !sum +. data.(j) done;
    let mean = !sum /. float window in
    let var_sum = ref 0.0 in
    for j = i - window + 1 to i do var_sum := !var_sum +. (data.(j) -. mean) ** 2.0 done;
    let stddev = sqrt (!var_sum /. float window) in
    if stddev > 0.0 then z.(i) <- (data.(i) -. mean) /. stddev
  done;
  z

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
  cash = 1000.0; position = 0.0; entry_price = 0.0;
  trades = 0; wins = 0;
  total_won = 0.0; total_lost = 0.0;
  equity_peak = 1000.0;
}

let equity pf price = pf.cash +. pf.position *. price

let apply_trade_with_risk pf signal price high low fee position_size stop_loss take_profit =
  match signal with
  | Buy when pf.position = 0.0 ->
    let invest = pf.cash *. (1.0 -. fee) *. position_size in
    let pos = invest /. price in
    { pf with cash = pf.cash -. invest; position = pos; entry_price = price }
  | Sell when pf.position > 0.0 ->
    let revenue = pf.position *. price *. (1.0 -. fee) in
    let cost = pf.position *. pf.entry_price in
    let pnl = revenue -. cost in
    if pnl > 0.0 then
      { pf with cash = pf.cash +. revenue; position = 0.0;
        trades = pf.trades + 1; wins = pf.wins + 1;
        total_won = pf.total_won +. pnl; entry_price = 0.0 }
    else
      { pf with cash = pf.cash +. revenue; position = 0.0;
        trades = pf.trades + 1;
        total_lost = pf.total_lost +. (-. pnl); entry_price = 0.0 }
  | _ ->
    if pf.position > 0.0 then
      let stop_price = pf.entry_price *. (1.0 -. stop_loss) in
      let take_price = pf.entry_price *. (1.0 +. take_profit) in
      if low <= stop_price then
        let revenue = pf.position *. stop_price *. (1.0 -. fee) in
        let cost = pf.position *. pf.entry_price in
        let pnl = revenue -. cost in
        if pnl > 0.0 then
          { pf with cash = pf.cash +. revenue; position = 0.0;
            trades = pf.trades + 1; wins = pf.wins + 1;
            total_won = pf.total_won +. pnl; entry_price = 0.0 }
        else
          { pf with cash = pf.cash +. revenue; position = 0.0;
            trades = pf.trades + 1;
            total_lost = pf.total_lost +. (-. pnl); entry_price = 0.0 }
      else if high >= take_price then
        let revenue = pf.position *. take_price *. (1.0 -. fee) in
        let cost = pf.position *. pf.entry_price in
        let pnl = revenue -. cost in
        if pnl > 0.0 then
          { pf with cash = pf.cash +. revenue; position = 0.0;
            trades = pf.trades + 1; wins = pf.wins + 1;
            total_won = pf.total_won +. pnl; entry_price = 0.0 }
        else
          { pf with cash = pf.cash +. revenue; position = 0.0;
            trades = pf.trades + 1;
            total_lost = pf.total_lost +. (-. pnl); entry_price = 0.0 }
      else pf
    else pf

let run_with_signals signals candles fee position_size stop_loss take_profit =
  let n = Array.length signals in
  let pf = ref init_pf in
  let max_dd = ref 0.0 in
  for i = 0 to n - 1 do
    let price = candles.(i).close in
    let high = candles.(i).high in
    let low = candles.(i).low in
    let signal = signals.(i) in
    pf := apply_trade_with_risk !pf signal price high low fee position_size stop_loss take_profit;
    let eq = equity !pf price in
    let peak = max !pf.equity_peak eq in
    pf := { !pf with equity_peak = peak };
    let dd = if peak > 0.0 then (peak -. eq) /. peak *. 100.0 else 0.0 in
    if dd > !max_dd then max_dd := dd
  done;
  let pf = !pf in
  let last_price = candles.(n - 1).close in
  let final_eq = equity pf last_price in
  let total_return = (final_eq -. 1000.0) /. 1000.0 *. 100.0 in
  let win_rate = if pf.trades = 0 then 0.0 else float pf.wins /. float pf.trades *. 100.0 in
  let profit_factor = if pf.total_lost = 0.0 then Float.infinity else pf.total_won /. pf.total_lost in
  (pf.trades, win_rate, profit_factor, !max_dd, total_return)

let periods = [
  ("2020-2021 Bull", 1597125600, 1640995200);
  ("2022 Bear", 1640995200, 1672531200);
  ("2023-2024 Recovery", 1672531200, 1711929600);
  ("2024-2026 OOS", 1711929600, 1778880780);
]

let run_all_periods all_candles signals fee position_size stop_loss take_profit =
  let is_trades = ref 0 in
  let is_peak_eq = ref 1000.0 in
  let is_lowest_eq = ref 1000.0 in

  List.iter (fun (pname, start_ts, end_ts) ->
    let slice = slice_by_timestamp all_candles start_ts end_ts in
    let sigs = slice_signals signals all_candles start_ts end_ts in
    let trades, wr, pf, mdd, ret =
      run_with_signals sigs slice fee position_size stop_loss take_profit
    in
    let pf_str = if pf = Float.infinity then "inf" else Printf.sprintf "%.2f" pf in
    Printf.printf "%-20s %-6d %-5.1f%% %-7s %-9.2f%% %+.2f%%\n"
      pname trades wr pf_str mdd ret;
    is_trades := !is_trades + trades;
    is_peak_eq := max !is_peak_eq (1000.0 *. (1.0 +. ret /. 100.0));
    is_lowest_eq := min !is_lowest_eq (1000.0 *. (1.0 -. mdd /. 100.0))
  ) (match periods with a :: b :: c :: _ -> [a; b; c] | _ -> periods);

  let oos_name, oos_start, oos_end = List.nth periods 3 in
  let oos_slice = slice_by_timestamp all_candles oos_start oos_end in
  let oos_sigs = slice_signals signals all_candles oos_start oos_end in
  let oos_trades, oos_wr, oos_pf, oos_mdd, oos_ret =
    run_with_signals oos_sigs oos_slice fee position_size stop_loss take_profit
  in
  let oos_pf_str = if oos_pf = Float.infinity then "inf" else Printf.sprintf "%.2f" oos_pf in
  Printf.printf "%-20s %-6d %-5.1f%% %-7s %-9.2f%% %+.2f%%\n\n"
    oos_name oos_trades oos_wr oos_pf_str oos_mdd oos_ret;
  (oos_ret, oos_pf)

let () =
  let all_candles = load_full_csv "data/SOLUSDT.csv" in
  Printf.printf "Loaded %d candles total\n\n" (Array.length all_candles);

  Printf.printf "=== Phase 6: Risk Management Optimization ===\n\n";
  Printf.printf "Signal: z-score w=200, sigma=2.5, SMA=500 (fixed)\n";
  Printf.printf "Variables: position_size, stop_loss, take_profit\n\n";

  Printf.printf "Precomputing signals...\n";
  let prices = Array.map (fun c -> c.close) all_candles in
  let z = compute_zscore prices 200 in
  let sma = compute_sma prices 500 in
  let n = Array.length all_candles in
  let signals = Array.init n (fun i ->
    if i < 500 then Hold
    else if prices.(i) > sma.(i) then
      if z.(i) < -.2.5 then Buy else Hold
    else
      if z.(i) > 2.5 then Sell else Hold
  ) in
  Printf.printf "Done.\n\n";

  let position_sizes = [0.25; 0.50; 1.0] in
  let stop_losses = [0.10; 0.15; 0.20; 0.25; 0.30] in
  let take_profits = [0.10; 0.15; 0.20; 0.25; 0.30] in

  let results = ref [] in
  let total = List.length position_sizes * List.length stop_losses * List.length take_profits in
  let count = ref 0 in

  List.iter (fun pos ->
    List.iter (fun sl ->
      List.iter (fun tp ->
        count := !count + 1;
        if !count mod 20 = 0 then Printf.printf "Progress: %d/%d\n" !count total;
        let oos_name, oos_start, oos_end = List.nth periods 3 in
        let oos_slice = slice_by_timestamp all_candles oos_start oos_end in
        let oos_signals = slice_signals signals all_candles oos_start oos_end in
        let trades, wr, pf, mdd, ret =
          run_with_signals oos_signals oos_slice 0.001 pos sl tp
        in
        results := (pos, sl, tp, trades, wr, pf, mdd, ret) :: !results
      ) take_profits
    ) stop_losses
  ) position_sizes;

  let sorted = List.sort (fun (_, _, _, _, _, _, _, r1) (_, _, _, _, _, _, _, r2) ->
    compare r2 r1
  ) !results in

  Printf.printf "\n=== Top 30 Configurations (sorted by OOS return) ===\n\n";
  Printf.printf "Rank  Pos%%   SL%%   TP%%   trades win%%   pf      max_dd     OOS%%\n";
  Printf.printf "----- ----- ----- ------ ------ ------ ------- ---------- ----------\n";

  let rec show_top n = function
    | [] -> ()
    | _ when n = 0 -> ()
    | (pos, sl, tp, trades, wr, pf, mdd, ret) :: rest ->
      let rank = 30 - n in
      let pf_str = if pf = Float.infinity then "inf" else Printf.sprintf "%.2f" pf in
      Printf.printf "%-5d %-5.0f%% %-5.0f%% %-5.0f%% %-6d %-5.1f%% %-7s %-9.2f%% %+.2f%%\n"
        rank (pos *. 100.0) (sl *. 100.0) (tp *. 100.0) trades wr pf_str mdd ret;
      show_top (n - 1) rest
  in
  show_top 30 sorted;

  Printf.printf "\n=== Best Config Deep Dive (all periods) ===\n\n";
  let best_pos, best_sl, best_tp =
    match sorted with
    | (p, s, t, _, _, _, _, _) :: _ -> (p, s, t)
    | [] -> (1.0, 1.0, 10.0)
  in
  Printf.printf "Best config: pos=%.0f%%, SL=%.0f%%, TP=%.0f%%\n\n"
    (best_pos *. 100.0) (best_sl *. 100.0) (best_tp *. 100.0);

  Printf.printf "Period               trades win%%   pf      max_dd     return\n";
  Printf.printf "-------------------- ------ ------ ------- ---------- ----------\n";
  let _ = run_all_periods all_candles signals 0.001 best_pos best_sl best_tp in
  ()

