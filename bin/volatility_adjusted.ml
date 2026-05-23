(* Phase 5: Volatility-Adjusted Thresholds *)
(* Tests if dynamic thresholds based on ATR improve robustness, especially in bear markets *)

type candle_data = {
  ts : int;
  open_price : float;
  close : float;
  high : float;
  low : float;
}

let load_full_csv path =
  let ic = open_in path in
  let candles = ref [] in
  let rec loop () =
    try
      let line = input_line ic in
      match String.split_on_char '|' line with
      | ts :: o :: h :: l :: close :: _rest ->
        (try
          let ts = int_of_string ts in
          let open_p = float_of_string o in
          let close = float_of_string close in
          let high = float_of_string h in
          let low = float_of_string l in
          candles := { ts; open_price = open_p; close; high; low } :: !candles
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

let compute_atr candles period =
  let n = Array.length candles in
  if n = 0 then [||]
  else
    let tr = Array.make n 0.0 in
    for i = 1 to n - 1 do
      let hl = candles.(i).high -. candles.(i).low in
      let hc = abs_float (candles.(i).high -. candles.(i - 1).close) in
      let lc = abs_float (candles.(i).low -. candles.(i - 1).close) in
      tr.(i) <- max hl (max hc lc)
    done;
    let atr = Array.make n 0.0 in
    let init = ref 0.0 in
    for i = 1 to period do init := !init +. tr.(i) done;
    atr.(period) <- !init /. float period;
    for i = period + 1 to n - 1 do
      atr.(i) <- (atr.(i - 1) *. (float period -. 1.0) +. tr.(i)) /. float period
    done;
    atr

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

type signal = Buy | Sell | Hold

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

let apply_trade pf signal price fee =
  match signal with
  | Buy when pf.position = 0.0 ->
    let invest = pf.cash *. (1.0 -. fee) in
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
  | _ -> pf

(* Baseline: fixed z-score threshold *)
let run_baseline candles window threshold sma_period fee =
  let prices = Array.map (fun c -> c.close) candles in
  let n = Array.length prices in
  if n < max window sma_period then (0, 0.0, 0.0, 0.0, 0.0)
  else
    let z = compute_zscore prices window in
    let sma = compute_sma prices sma_period in
    let pf = ref init_pf in
    let max_dd = ref 0.0 in
    for i = max window sma_period to n - 1 do
      let price = prices.(i) in
      let signal =
        if price > sma.(i) then
          if z.(i) < -.threshold then Buy else Hold
        else
          if z.(i) > threshold then Sell else Hold
      in
      pf := apply_trade !pf signal price fee;
      let eq = equity !pf price in
      let peak = max !pf.equity_peak eq in
      pf := { !pf with equity_peak = peak };
      let dd = if peak > 0.0 then (peak -. eq) /. peak *. 100.0 else 0.0 in
      if dd > !max_dd then max_dd := dd
    done;
    let pf = !pf in
    let last_price = prices.(n - 1) in
    let final_eq = equity pf last_price in
    let total_return = (final_eq -. 1000.0) /. 1000.0 *. 100.0 in
    let win_rate = if pf.trades = 0 then 0.0 else float pf.wins /. float pf.trades *. 100.0 in
    let profit_factor = if pf.total_lost = 0.0 then Float.infinity else pf.total_won /. pf.total_lost in
    (pf.trades, win_rate, profit_factor, !max_dd, total_return)

(* Volatility-adjusted: threshold scales with ATR ratio *)
let run_vol_adjusted candles window base_threshold sma_period atr_period fee vol_multiplier =
  let prices = Array.map (fun c -> c.close) candles in
  let n = Array.length prices in
  let warmup = max window (max sma_period (max atr_period 100)) in
  if n < warmup then (0, 0.0, 0.0, 0.0, 0.0)
  else
    let z = compute_zscore prices window in
    let sma = compute_sma prices sma_period in
    let atr = compute_atr candles atr_period in
    let avg_atr = compute_sma atr 100 in
    let pf = ref init_pf in
    let max_dd = ref 0.0 in
    for i = warmup to n - 1 do
      let price = prices.(i) in
      let atr_ratio = if avg_atr.(i) > 0.0 then atr.(i) /. avg_atr.(i) else 1.0 in
      let dynamic_threshold = base_threshold *. (1.0 +. vol_multiplier *. (atr_ratio -. 1.0)) in
      let signal =
        if price > sma.(i) then
          if z.(i) < -.dynamic_threshold then Buy else Hold
        else
          if z.(i) > dynamic_threshold then Sell else Hold
      in
      pf := apply_trade !pf signal price fee;
      let eq = equity !pf price in
      let peak = max !pf.equity_peak eq in
      pf := { !pf with equity_peak = peak };
      let dd = if peak > 0.0 then (peak -. eq) /. peak *. 100.0 else 0.0 in
      if dd > !max_dd then max_dd := dd
    done;
    let pf = !pf in
    let last_price = prices.(n - 1) in
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

let () =
  let all_candles = load_full_csv "data/SOLUSDT.csv" in
  Printf.printf "Loaded %d candles total\n\n" (Array.length all_candles);

  Printf.printf "=== Phase 5: Volatility-Adjusted Thresholds ===\n\n";

  Printf.printf "Hypothesis: Dynamic thresholds (ATR-based) reduce drawdown in volatile periods\n";
  Printf.printf "while maintaining performance in calm markets.\n\n";

  (* Test baseline vs vol-adjusted across all periods *)
  let configs = [
    ("Baseline: w=200,σ=2.5,SMA=500",
     fun candles -> run_baseline candles 200 2.5 500 0.001);
    ("VolAdj: w=200,σ=2.0,ATR×0.5,SMA=500",
     fun candles -> run_vol_adjusted candles 200 2.0 500 14 0.001 0.5);
    ("VolAdj: w=200,σ=2.0,ATR×1.0,SMA=500",
     fun candles -> run_vol_adjusted candles 200 2.0 500 14 0.001 1.0);
    ("VolAdj: w=200,σ=2.0,ATR×1.5,SMA=500",
     fun candles -> run_vol_adjusted candles 200 2.0 500 14 0.001 1.5);
    ("VolAdj: w=200,σ=2.5,ATR×1.0,SMA=500",
     fun candles -> run_vol_adjusted candles 200 2.5 500 14 0.001 1.0);
    ("VolAdj: w=200,σ=2.5,ATR×2.0,SMA=500",
     fun candles -> run_vol_adjusted candles 200 2.5 500 14 0.001 2.0);
  ] in

  List.iter (fun (name, run_fn) ->
    Printf.printf "Strategy: %s\n" name;
    Printf.printf "Period               trades win%%   pf      max_dd     return\n";
    Printf.printf "──────────────────── ────── ────── ─────── ─────────  ──────────\n";

    let is_trades = ref 0 in
    let is_wins = ref 0 in
    let is_peak_eq = ref 1000.0 in
    let is_lowest_eq = ref 1000.0 in

    List.iter (fun (pname, start_ts, end_ts) ->
      let slice = slice_by_timestamp all_candles start_ts end_ts in
      let trades, wr, pf, mdd, ret = run_fn slice in
      let pf_str = if pf = Float.infinity then "inf" else Printf.sprintf "%.2f" pf in
      Printf.printf "%-20s %-6d %-5.1f%% %-7s %-9.2f%% %+.2f%%\n"
        pname trades wr pf_str mdd ret;
      is_trades := !is_trades + trades;
      is_wins := !is_wins + int_of_float (float trades *. wr /. 100.0);
      is_peak_eq := max !is_peak_eq (1000.0 *. (1.0 +. ret /. 100.0));
      is_lowest_eq := min !is_lowest_eq (1000.0 *. (1.0 -. mdd /. 100.0))
    ) [List.nth periods 0; List.nth periods 1; List.nth periods 2];

    let oos_name, oos_start, oos_end = List.nth periods 3 in
    let oos_slice = slice_by_timestamp all_candles oos_start oos_end in
    let oos_trades, oos_wr, oos_pf, oos_mdd, oos_ret = run_fn oos_slice in
    let oos_pf_str = if oos_pf = Float.infinity then "inf" else Printf.sprintf "%.2f" oos_pf in

    Printf.printf "%-20s %-6d %-5.1f%% %-7s %-9.2f%% %+.2f%%\n\n"
      oos_name oos_trades oos_wr oos_pf_str oos_mdd oos_ret
  ) configs;

  Printf.printf "=== Analysis ===\n\n";
  Printf.printf "VolAdj formula: dynamic_threshold = base_threshold * (1 + vol_mult * (ATR/avgATR - 1))\n";
  Printf.printf "When ATR > avg: threshold widens (fewer signals, avoid whipsaws)\n";
  Printf.printf "When ATR < avg: threshold tightens (more signals, capture mean reversion)\n\n";
  Printf.printf "Key question: Does VolAdj reduce 2022 Bear drawdown vs baseline?\n"
