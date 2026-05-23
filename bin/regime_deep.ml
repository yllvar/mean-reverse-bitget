(* Phase 2 Deep Dive: Fine-grained ADX thresholds + skip-trending approach *)

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

let compute_adx candles period =
  let n = Array.length candles in
  let tr = Array.make n 0.0 in
  let plus_dm = Array.make n 0.0 in
  let minus_dm = Array.make n 0.0 in
  for i = 1 to n - 1 do
    let high = candles.(i).high in
    let low = candles.(i).low in
    let prev_close = candles.(i - 1).close in
    tr.(i) <- max (high -. low) (max (abs_float (high -. prev_close)) (abs_float (low -. prev_close)));
    let up_move = candles.(i).high -. candles.(i - 1).high in
    let down_move = candles.(i - 1).low -. candles.(i).low in
    if up_move > down_move && up_move > 0.0 then plus_dm.(i) <- up_move;
    if down_move > up_move && down_move > 0.0 then minus_dm.(i) <- down_move
  done;
  let smooth_tr = Array.make n 0.0 in
  let smooth_plus = Array.make n 0.0 in
  let smooth_minus = Array.make n 0.0 in
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
  for i = period + 1 to n - 1 do
    smooth_tr.(i) <- (smooth_tr.(i - 1) *. (float period -. 1.0) +. tr.(i)) /. float period;
    smooth_plus.(i) <- (smooth_plus.(i - 1) *. (float period -. 1.0) +. plus_dm.(i)) /. float period;
    smooth_minus.(i) <- (smooth_minus.(i - 1) *. (float period -. 1.0) +. minus_dm.(i)) /. float period
  done;
  let plus_di = Array.make n 0.0 in
  let minus_di = Array.make n 0.0 in
  let dx = Array.make n 0.0 in
  for i = period to n - 1 do
    if smooth_tr.(i) > 0.0 then begin
      plus_di.(i) <- smooth_plus.(i) /. smooth_tr.(i) *. 100.0;
      minus_di.(i) <- smooth_minus.(i) /. smooth_tr.(i) *. 100.0
    end;
    let di_sum = plus_di.(i) +. minus_di.(i) in
    if di_sum > 0.0 then dx.(i) <- abs_float (plus_di.(i) -. minus_di.(i)) /. di_sum *. 100.0
  done;
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

(* Strategy A: ADX regime with order flow *)
(* mode = `TrendOnly: only trade in trending (buy on pullbacks) *)
(* mode = `RangeOnly: only trade in ranging (mean revert) *)
(* mode = `Dual: trend=pullback buys, range=mean revert both ways *)
let backtest_adx_regime candles adx_threshold mode imb_ema_period buy_threshold sell_threshold fee =
  let n = Array.length candles in
  let imbalances = Array.init n (fun i -> compute_imbalance candles.(i)) in
  let ema_imb = compute_ema imbalances imb_ema_period in
  let adx = compute_adx candles 14 in

  let pf = ref init_pf in
  let max_dd = ref 0.0 in
  let trend_count = ref 0 in
  let range_count = ref 0 in

  for i = 200 to n - 1 do
    let price = candles.(i).close in
    let is_trending = adx.(i) > adx_threshold in
    if is_trending then incr trend_count else incr range_count;

    let signal =
      if is_trending then
        match mode with
        | `TrendOnly -> if ema_imb.(i) < -.buy_threshold then Buy else Hold
        | `Dual -> if ema_imb.(i) < -.buy_threshold then Buy else Hold
        | `RangeOnly -> Hold
      else
        match mode with
        | `TrendOnly -> Hold
        | `Dual -> if ema_imb.(i) < -.buy_threshold then Buy
                   else if ema_imb.(i) > sell_threshold then Sell
                   else Hold
        | `RangeOnly -> if ema_imb.(i) < -.buy_threshold then Buy
                        else if ema_imb.(i) > sell_threshold then Sell
                        else Hold
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
  (pf.trades, win_rate, profit_factor, !max_dd, total_return, !trend_count, !range_count)

(* Strategy B: SMA deviation regime with order flow *)
let backtest_dev_regime candles dev_threshold mode imb_ema_period buy_threshold sell_threshold fee =
  let n = Array.length candles in
  let imbalances = Array.init n (fun i -> compute_imbalance candles.(i)) in
  let ema_imb = compute_ema imbalances imb_ema_period in
  let sma_dev = compute_sma_deviation (Array.map (fun c -> c.close) candles) 200 in

  let pf = ref init_pf in
  let max_dd = ref 0.0 in
  let trend_count = ref 0 in
  let range_count = ref 0 in

  for i = 200 to n - 1 do
    let price = candles.(i).close in
    let is_trending = sma_dev.(i) > dev_threshold in
    if is_trending then incr trend_count else incr range_count;

    let signal =
      if is_trending then
        match mode with
        | `TrendOnly -> if ema_imb.(i) < -.buy_threshold then Buy else Hold
        | `Dual -> if ema_imb.(i) < -.buy_threshold then Buy else Hold
        | `RangeOnly -> Hold
      else
        match mode with
        | `TrendOnly -> Hold
        | `Dual -> if ema_imb.(i) < -.buy_threshold then Buy
                   else if ema_imb.(i) > sell_threshold then Sell
                   else Hold
        | `RangeOnly -> if ema_imb.(i) < -.buy_threshold then Buy
                        else if ema_imb.(i) > sell_threshold then Sell
                        else Hold
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
  (pf.trades, win_rate, profit_factor, !max_dd, total_return, !trend_count, !range_count)

(* Strategy C: Pure trend-following (buy & hold when price > SMA, flat otherwise) *)
let backtest_trend_follow candles sma_period fee =
  let n = Array.length candles in
  let prices = Array.map (fun c -> c.close) candles in
  let pf = ref init_pf in
  let max_dd = ref 0.0 in
  let window = Array.make sma_period 0.0 in
  let sum = ref 0.0 in
  let idx = ref 0 in

  for i = 0 to sma_period - 1 do
    window.(i) <- prices.(i);
    sum := !sum +. prices.(i)
  done;

  for i = sma_period to n - 1 do
    let price = candles.(i).close in
    let sma = !sum /. float sma_period in
    let signal = if price > sma then Buy else Sell in
    pf := apply_trade !pf signal price fee;
    let eq = equity !pf price in
    let peak = max !pf.equity_peak eq in
    pf := { !pf with equity_peak = peak };
    let dd = if peak > 0.0 then (peak -. eq) /. peak *. 100.0 else 0.0 in
    if dd > !max_dd then max_dd := dd;
    let old = window.(!idx) in
    window.(!idx) <- price;
    sum := !sum -. old +. price;
    idx := (!idx + 1) mod sma_period
  done;

  let pf = !pf in
  let last_price = candles.(n - 1).close in
  let final_eq = equity pf last_price in
  let total_return = (final_eq -. 1000.0) /. 1000.0 *. 100.0 in
  let win_rate = if pf.trades = 0 then 0.0 else float pf.wins /. float pf.trades *. 100.0 in
  let profit_factor = if pf.total_lost = 0.0 then Float.infinity else pf.total_won /. pf.total_lost in
  (pf.trades, win_rate, profit_factor, !max_dd, total_return)

let fee = 0.001

let () =
  let csv_path =
    try Sys.argv.(1) with _ -> "data/SOLUSDT.csv"
  in
  let candles = load_full_csv csv_path in
  let n = Array.length candles in
  Printf.eprintf "Loaded %d candles\n%!" n;

  (* Fine-grained ADX thresholds with RangeOnly mode *)
  Printf.printf "\n=== ADX Range-Only (skip trending, mean-revert in ranges) ===\n";
  Printf.printf "%-12s %-6s %-6s %-7s %-9s  %s  (trend%%/range%%)\n%!"
    "adx_thresh" "trades" "win%" "pf" "max_dd" "return";
  List.iter (fun t ->
    let trades, wr, pf, mdd, ret, tc, rc =
      backtest_adx_regime candles t `RangeOnly 15 0.3 0.3 fee in
    let total = float (tc + rc) in
    Printf.printf "%-12.1f %-6d %-5.1f%% %-7.2f %-7.2f%% %+.2f%%  (%.1f%%/%.1f%%)\n%!"
      t trades wr pf mdd ret (float tc /. total *. 100.0) (float rc /. total *. 100.0)
  ) [10.0; 12.0; 14.0; 15.0; 16.0; 18.0; 20.0; 22.0; 25.0; 30.0];

  (* ADX Dual mode with fine thresholds *)
  Printf.printf "\n=== ADX Dual Mode (trend=pullback, range=mean-revert) ===\n";
  Printf.printf "%-12s %-6s %-6s %-7s %-9s  %s  (trend%%/range%%)\n%!"
    "adx_thresh" "trades" "win%" "pf" "max_dd" "return";
  List.iter (fun t ->
    let trades, wr, pf, mdd, ret, tc, rc =
      backtest_adx_regime candles t `Dual 15 0.5 0.3 fee in
    let total = float (tc + rc) in
    Printf.printf "%-12.1f %-6d %-5.1f%% %-7.2f %-7.2f%% %+.2f%%  (%.1f%%/%.1f%%)\n%!"
      t trades wr pf mdd ret (float tc /. total *. 100.0) (float rc /. total *. 100.0)
  ) [10.0; 12.0; 14.0; 15.0; 16.0; 18.0; 20.0; 22.0; 25.0; 30.0];

  (* ADX TrendOnly mode *)
  Printf.printf "\n=== ADX Trend-Only (buy pullbacks in trends only) ===\n";
  Printf.printf "%-12s %-6s %-6s %-7s %-9s  %s  (trend%%/range%%)\n%!"
    "adx_thresh" "trades" "win%" "pf" "max_dd" "return";
  List.iter (fun t ->
    let trades, wr, pf, mdd, ret, tc, rc =
      backtest_adx_regime candles t `TrendOnly 15 0.5 0.3 fee in
    let total = float (tc + rc) in
    Printf.printf "%-12.1f %-6d %-5.1f%% %-7.2f %-7.2f%% %+.2f%%  (%.1f%%/%.1f%%)\n%!"
      t trades wr pf mdd ret (float tc /. total *. 100.0) (float rc /. total *. 100.0)
  ) [10.0; 12.0; 14.0; 15.0; 16.0; 18.0; 20.0; 22.0; 25.0; 30.0];

  (* SMA Deviation Range-Only *)
  Printf.printf "\n=== SMA Dev Range-Only ===\n";
  Printf.printf "%-12s %-6s %-6s %-7s %-9s  %s  (trend%%/range%%)\n%!"
    "dev_thresh" "trades" "win%" "pf" "max_dd" "return";
  List.iter (fun t ->
    let trades, wr, pf, mdd, ret, tc, rc =
      backtest_dev_regime candles t `RangeOnly 15 0.3 0.3 fee in
    let total = float (tc + rc) in
    Printf.printf "%-12.3f %-6d %-5.1f%% %-7.2f %-7.2f%% %+.2f%%  (%.1f%%/%.1f%%)\n%!"
      t trades wr pf mdd ret (float tc /. total *. 100.0) (float rc /. total *. 100.0)
  ) [0.02; 0.03; 0.05; 0.08; 0.10; 0.15];

  (* Pure trend following baseline *)
  Printf.printf "\n=== Pure Trend Following (buy when price > SMA) ===\n";
  Printf.printf "%-6s %-6s %-6s %-7s %-9s  %s\n%!"
    "sma" "trades" "win%" "pf" "max_dd" "return";
  List.iter (fun sma ->
    let trades, wr, pf, mdd, ret = backtest_trend_follow candles sma fee in
    Printf.printf "%-6d %-6d %-5.1f%% %-7.2f %-7.2f%% %+.2f%%\n%!"
      sma trades wr pf mdd ret
  ) [50; 100; 200; 500]
