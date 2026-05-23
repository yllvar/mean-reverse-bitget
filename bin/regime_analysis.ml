(* Phase 2: Regime Detection Analysis & Backtest *)

type candle_data = {
  close : float;
  high : float;
  low : float;
  base_volume : float;
  taker_buy_base : float;
}

let load_full_csv path =
  let ic = open_in path in
  let candles = ref [] in
  let rec loop () =
    try
      let line = input_line ic in
      match String.split_on_char '|' line with
      | _ts :: o :: h :: l :: close :: base_vol :: _qv :: taker_buy :: _rest ->
        (try
          let close = float_of_string close in
          let high = float_of_string h in
          let low = float_of_string l in
          let base_vol = float_of_string base_vol in
          let taker_buy = float_of_string taker_buy in
          if base_vol > 0.0 then
            candles := { close; high; low; base_volume = base_vol; taker_buy_base = taker_buy } :: !candles
         with Failure _ -> ());
          loop ()
      | _ -> loop ()
    with End_of_file -> close_in ic
  in
  loop ();
  Array.of_list (List.rev !candles)

(* === REGIME INDICATORS === *)

(* 1. SMA Deviation: |price - SMA(N)| / SMA(N) *)
let compute_sma_deviation prices sma_period =
  let n = Array.length prices in
  let dev = Array.make n 0.0 in
  let window = Array.make sma_period 0.0 in
  let sum = ref 0.0 in
  let idx = ref 0 in

  for i = 0 to sma_period - 1 do
    window.(i) <- prices.(i);
    sum := !sum +. prices.(i)
  done;

  for i = sma_period to n - 1 do
    let sma = !sum /. float sma_period in
    dev.(i) <- abs_float (prices.(i) -. sma) /. sma;
    let old = window.(!idx) in
    window.(!idx) <- prices.(i);
    sum := !sum -. old +. prices.(i);
    idx := (!idx + 1) mod sma_period
  done;
  dev

(* 2. ADX - corrected Wilder's smoothing *)
let compute_adx candles period =
  let n = Array.length candles in
  let tr = Array.make n 0.0 in
  let plus_dm = Array.make n 0.0 in
  let minus_dm = Array.make n 0.0 in

  (* Step 1: Calculate TR and DM *)
  for i = 1 to n - 1 do
    let high = candles.(i).high in
    let low = candles.(i).low in
    let prev_close = candles.(i - 1).close in
    let tr1 = high -. low in
    let tr2 = abs_float (high -. prev_close) in
    let tr3 = abs_float (low -. prev_close) in
    tr.(i) <- max tr1 (max tr2 tr3);

    let up_move = candles.(i).high -. candles.(i - 1).high in
    let down_move = candles.(i - 1).low -. candles.(i).low in
    if up_move > down_move && up_move > 0.0 then plus_dm.(i) <- up_move;
    if down_move > up_move && down_move > 0.0 then minus_dm.(i) <- down_move
  done;

  (* Step 2: Wilder's smoothing (RMA) for TR, +DM, -DM *)
  let smooth_tr = Array.make n 0.0 in
  let smooth_plus = Array.make n 0.0 in
  let smooth_minus = Array.make n 0.0 in

  (* Initial SMA for first 'period' values *)
  let init_tr = ref 0.0 in
  let init_plus = ref 0.0 in
  let init_minus = ref 0.0 in
  for i = 1 to period do
    init_tr := !init_tr +. tr.(i);
    init_plus := !init_plus +. plus_dm.(i);
    init_minus := !init_minus +. minus_dm.(i)
  done;
  smooth_tr.(period) <- !init_tr;
  smooth_plus.(period) <- !init_plus;
  smooth_minus.(period) <- !init_minus;

  (* Wilder's smoothing: S[i] = S[i-1] - S[i-1]/period + value[i] *)
  for i = period + 1 to n - 1 do
    smooth_tr.(i) <- (smooth_tr.(i - 1) *. (float period -. 1.0) +. tr.(i)) /. float period;
    smooth_plus.(i) <- (smooth_plus.(i - 1) *. (float period -. 1.0) +. plus_dm.(i)) /. float period;
    smooth_minus.(i) <- (smooth_minus.(i - 1) *. (float period -. 1.0) +. minus_dm.(i)) /. float period
  done;

  (* Step 3: Calculate +DI, -DI, DX *)
  let plus_di = Array.make n 0.0 in
  let minus_di = Array.make n 0.0 in
  let dx = Array.make n 0.0 in

  for i = period to n - 1 do
    if smooth_tr.(i) > 0.0 then begin
      plus_di.(i) <- smooth_plus.(i) /. smooth_tr.(i) *. 100.0;
      minus_di.(i) <- smooth_minus.(i) /. smooth_tr.(i) *. 100.0
    end;
    let di_sum = plus_di.(i) +. minus_di.(i) in
    if di_sum > 0.0 then
      dx.(i) <- abs_float (plus_di.(i) -. minus_di.(i)) /. di_sum *. 100.0
  done;

  (* Step 4: Smooth DX to get ADX *)
  let adx = Array.make n 0.0 in
  let init_dx = ref 0.0 in
  for i = period to 2 * period - 1 do
    init_dx := !init_dx +. dx.(i)
  done;
  adx.(2 * period - 1) <- !init_dx /. float period;

  for i = 2 * period to n - 1 do
    adx.(i) <- (adx.(i - 1) *. (float period -. 1.0) +. dx.(i)) /. float period
  done;

  adx

(* === PORTFOLIO & TRADING === *)

type signal = Buy | Sell | Hold
type regime = Trending | Ranging | Transition

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

let apply_trade pf signal price fee =
  match signal with
  | Buy when pf.position = 0.0 ->
    let pos = pf.cash *. (1.0 -. fee) /. price in
    { pf with cash = 0.0; position = pos; entry_value = pf.cash }
  | Sell when pf.position > 0.0 ->
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

(* === ORDER FLOW HELPERS === *)

let compute_imbalance c =
  if c.base_volume <= 0.0 then 0.0
  else (c.taker_buy_base -. (c.base_volume -. c.taker_buy_base)) /. c.base_volume

let compute_ema data period =
  let n = Array.length data in
  if period <= 0 || n = 0 then data
  else
    let ema = Array.make n 0.0 in
    let multiplier = 2.0 /. (float period +. 1.0) in
    ema.(0) <- data.(0);
    for i = 1 to n - 1 do
      ema.(i) <- (data.(i) -. ema.(i - 1)) *. multiplier +. ema.(i - 1)
    done;
    ema

(* === DUAL STRATEGY: Order Flow + Regime === *)

let backtest_dual candles regime_type adx_threshold dev_threshold fee =
  let n = Array.length candles in
  let prices = Array.map (fun c -> c.close) candles in
  let imbalances = Array.init n (fun i -> compute_imbalance candles.(i)) in
  let ema_imb = compute_ema imbalances 15 in

  let adx = compute_adx candles 14 in
  let sma_dev = compute_sma_deviation prices 200 in

  let pf = ref init_pf in
  let max_dd = ref 0.0 in
  let trend_trades = ref 0 in
  let range_trades = ref 0 in
  let trans_trades = ref 0 in

  for i = 200 to n - 1 do
    let price = candles.(i).close in
    let regime = match regime_type with
      | `ADX ->
        if adx.(i) > adx_threshold then Trending
        else if adx.(i) > adx_threshold -. 5.0 then Transition
        else Ranging
      | `Deviation ->
        if sma_dev.(i) > dev_threshold then Trending
        else if sma_dev.(i) > dev_threshold /. 2.0 then Transition
        else Ranging
    in

    let signal = match regime with
      | Trending ->
        incr trend_trades;
        if ema_imb.(i) < -0.5 then Buy else Hold
      | Ranging ->
        incr range_trades;
        if ema_imb.(i) < -0.3 then Buy
        else if ema_imb.(i) > 0.3 then Sell
        else Hold
      | Transition ->
        incr trans_trades;
        Hold
    in

    pf := apply_trade !pf signal price fee;
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
  (pf.trades, win_rate, profit_factor, !max_dd, total_return,
   !trend_trades, !range_trades, !trans_trades)

(* === REGIME STATISTICS === *)

let analyze_regime_stats candles =
  let n = Array.length candles in
  let prices = Array.map (fun c -> c.close) candles in
  let adx = compute_adx candles 14 in
  let sma_dev = compute_sma_deviation prices 200 in

  (* ADX distribution *)
  let adx_buckets = Array.make 11 0 in
  let adx_sum = ref 0.0 in
  let adx_count = ref 0 in
  let adx_min = ref 100.0 in
  let adx_max = ref 0.0 in

  for i = 28 to n - 1 do
    let v = adx.(i) in
    if v > 0.0 then begin
      adx_sum := !adx_sum +. v;
      incr adx_count;
      if v < !adx_min then adx_min := v;
      if v > !adx_max then adx_max := v;
      let bucket = min 10 (int_of_float (v /. 10.0)) in
      adx_buckets.(bucket) <- adx_buckets.(bucket) + 1
    end
  done;

  Printf.printf "\n=== ADX Distribution ===\n";
  Printf.printf "Min: %.2f  Max: %.2f  Mean: %.2f (over %d non-zero)\n"
    !adx_min !adx_max (!adx_sum /. float !adx_count) !adx_count;
  for i = 0 to 10 do
    Printf.printf "%2d-%2d: %8d (%.1f%%)\n" (i*10) ((i+1)*10) adx_buckets.(i)
      (float adx_buckets.(i) /. float !adx_count *. 100.0)
  done;

  (* SMA Deviation distribution *)
  let dev_buckets = Array.make 6 0 in
  for i = 200 to n - 1 do
    let v = sma_dev.(i) in
    let bucket = min 5 (int_of_float (v /. 0.05)) in
    dev_buckets.(bucket) <- dev_buckets.(bucket) + 1
  done;

  let dev_count = n - 200 in
  Printf.printf "\n=== SMA(200) Deviation Distribution ===\n";
  for i = 0 to 4 do
    Printf.printf "%.2f-%.2f: %8d (%.1f%%)\n" (float i *. 0.05) (float (i+1) *. 0.05) dev_buckets.(i)
      (float dev_buckets.(i) /. float dev_count *. 100.0)
  done;
  Printf.printf "0.25+:   %8d (%.1f%%)\n" dev_buckets.(5)
    (float dev_buckets.(5) /. float dev_count *. 100.0)

let fee = 0.001

let () =
  let csv_path =
    try Sys.argv.(1) with _ -> "data/SOLUSDT.csv"
  in
  let candles = load_full_csv csv_path in
  let n = Array.length candles in
  Printf.eprintf "Loaded %d candles\n%!" n;

  (* Step 2.2: Regime Statistics *)
  analyze_regime_stats candles;

  (* Step 2.3: Dual Strategy with ADX *)
  Printf.printf "\n=== Dual Strategy: Order Flow + ADX Regime ===\n";
  Printf.printf "%-12s %-6s %-6s %-7s %-9s  %s  (T/R/X trades)\n%!"
    "adx_thresh" "trades" "win%" "pf" "max_dd" "return";
  List.iter (fun t ->
    let trades, wr, pf, mdd, ret, tt, rt, xt =
      backtest_dual candles `ADX t 0.0 fee in
    Printf.printf "%-12.1f %-6d %-5.1f%% %-7.2f %-7.2f%% %+.2f%%  (%d/%d/%d)\n%!"
      t trades wr pf mdd ret tt rt xt
  ) [15.0; 20.0; 25.0; 30.0; 35.0; 40.0];

  (* Step 2.4: Dual Strategy with SMA Deviation *)
  Printf.printf "\n=== Dual Strategy: Order Flow + SMA Deviation Regime ===\n";
  Printf.printf "%-12s %-6s %-6s %-7s %-9s  %s  (T/R/X trades)\n%!"
    "dev_thresh" "trades" "win%" "pf" "max_dd" "return";
  List.iter (fun t ->
    let trades, wr, pf, mdd, ret, tt, rt, xt =
      backtest_dual candles `Deviation 0.0 t fee in
    Printf.printf "%-12.3f %-6d %-5.1f%% %-7.2f %-7.2f%% %+.2f%%  (%d/%d/%d)\n%!"
      t trades wr pf mdd ret tt rt xt
  ) [0.02; 0.05; 0.10; 0.15; 0.20]
