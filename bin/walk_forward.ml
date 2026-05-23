(* Phase 4: Walk-Forward Validation + Sub-Period Analysis *)
(* Tests if best configs from Phase 1-3 survive out-of-sample data *)

type candle_data = {
  ts : int;
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
      | ts :: o :: h :: l :: close :: base_vol :: _qv :: taker_buy :: _rest ->
        (try
          let ts = int_of_string ts in
          let close = float_of_string close in
          let high = float_of_string h in
          let low = float_of_string l in
          let base_vol = float_of_string base_vol in
          let taker_buy = float_of_string taker_buy in
          if base_vol > 0.0 then
            candles := { ts; close; high; low; base_volume = base_vol; taker_buy_base = taker_buy } :: !candles
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

let compute_adx candles period =
  let n = Array.length candles in
  if n = 0 then [||]
  else
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

let compute_imbalance c =
  if c.base_volume <= 0.0 then 0.0
  else (c.taker_buy_base -. (c.base_volume -. c.taker_buy_base)) /. c.base_volume

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

let apply_trade pf signal price fee position_size =
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
  | _ -> pf

(* Strategy: ADX regime + order flow + optional time filter + position sizing *)
let run_strategy candles adx_threshold imb_ema buy_thresh sell_thresh
    fee position_size hour_filter =
  let n = Array.length candles in
  if n < 200 then (0, 0.0, 0.0, 0.0, 0.0)
  else
    let imbalances = Array.init n (fun i -> compute_imbalance candles.(i)) in
    let ema_imb = compute_ema imbalances imb_ema in
    let adx = compute_adx candles 14 in
    let pf = ref init_pf in
    let max_dd = ref 0.0 in
    for i = 200 to n - 1 do
      let price = candles.(i).close in
      let hour = (candles.(i).ts / 3600) mod 24 in
      let dow = (candles.(i).ts / 86400 + 4) mod 7 in
      let is_weekend = dow = 5 || dow = 6 in
      let is_trending = adx.(i) > adx_threshold in
      let pass_time = match hour_filter with
        | `All -> true
        | `LondonWeekday -> hour >= 8 && hour < 12 && not is_weekend
        | `LateWeekday -> hour >= 20 && hour < 24 && not is_weekend
        | `USWeekday -> hour >= 12 && hour < 20 && not is_weekend
      in
      if pass_time && not is_trending then
        let signal =
          if ema_imb.(i) < -.buy_thresh then Buy
          else if ema_imb.(i) > sell_thresh then Sell
          else Hold
        in
        pf := apply_trade !pf signal price fee position_size;
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

(* Strategy: Original z-score baseline *)
let run_zscore candles window_size threshold sma_period fee =
  let prices = Array.map (fun c -> c.close) candles in
  let n = Array.length prices in
  if n < window_size + (if sma_period > 0 then sma_period else 0) + 1 then (0, 0.0, 0.0, 0.0, 0.0)
  else
    let window = Array.make window_size 0.0 in
    let sma_win = Array.make sma_period 0.0 in
    for i = 0 to window_size - 1 do window.(i) <- prices.(i) done;
    if sma_period > 0 then for i = 0 to sma_period - 1 do sma_win.(i) <- prices.(i) done;
    let pf = ref init_pf in
    let max_dd = ref 0.0 in
    let w_idx = ref 0 in
    let s_idx = ref 0 in
    let w_sum = ref (Array.fold_left (+.) 0.0 window) in
    let w_sq = ref (Array.fold_left (fun a x -> a +. x *. x) 0.0 window) in
    let s_sum = ref (if sma_period > 0 then Array.fold_left (+.) 0.0 sma_win else 0.0) in
    let min_len = window_size + (if sma_period > 0 then sma_period else 0) + 1 in
    for i = min_len to n - 1 do
      let price = prices.(i) in
      let mean = !w_sum /. float window_size in
      let variance = !w_sq /. float window_size -. mean *. mean in
      let stddev = if variance > 0.0 then sqrt variance else 0.0 in
      let s =
        if stddev < 0.01 then Hold
        else
          let z = (price -. mean) /. stddev in
          if z < -.threshold then Buy
          else if z > threshold then Sell
          else Hold
      in
      let s =
        if sma_period <= 0 then s
        else
          let sma = !s_sum /. float sma_period in
          if sma = 0.0 then s
          else match s with
          | Buy when price <= sma -> Hold
          | Sell when price >= sma -> Hold
          | _ -> s
      in
      let price_f = price in
      pf := apply_trade !pf s price_f fee 1.0;
      let eq = equity !pf price_f in
      let peak = max !pf.equity_peak eq in
      pf := { !pf with equity_peak = peak };
      let dd = if peak > 0.0 then (peak -. eq) /. peak *. 100.0 else 0.0 in
      if dd > !max_dd then max_dd := dd;
      let old_w = window.(!w_idx) in
      window.(!w_idx) <- price;
      w_sum := !w_sum -. old_w +. price;
      w_sq := !w_sq -. old_w *. old_w +. price *. price;
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
    let win_rate = if pf.trades = 0 then 0.0 else float pf.wins /. float pf.trades *. 100.0 in
    let profit_factor = if pf.total_lost = 0.0 then Float.infinity else pf.total_won /. pf.total_lost in
    (pf.trades, win_rate, profit_factor, !max_dd, total_return)

let fee = 0.001

(* Period definitions *)
let periods = [
  ("2020-2021 Bull",   1597125600, 1640995200);  (* Aug 2020 - Jan 2022 *)
  ("2022 Bear",         1640995200, 1672531200);  (* Jan 2022 - Jan 2023 *)
  ("2023-2024 Recovery",1672531200, 1711929600);  (* Jan 2023 - Apr 2024 *)
  ("In-Sample Total",   1597125600, 1711929600);  (* Aug 2020 - Apr 2024 *)
  ("Out-Of-Sample",     1711929600, 1778880780);  (* Apr 2024 - May 2026 *)
]

(* Strategy definitions *)
type strategy = {
  name : string;
  runner : candle_data array -> (int * float * float * float * float);
}

let strategies = [
  (* Phase 0: Baseline z-score *)
  { name = "Baseline: w=200,σ=2.5,SMA=500";
    runner = fun c -> run_zscore c 200 2.5 500 fee };

  (* Phase 1: Order flow + SMA *)
  { name = "P1: OF EMA=15,th=0.5,SMA=200";
    runner = fun c -> run_strategy c 10.0 15 0.5 0.3 fee 1.0 `All };

  (* Phase 2: ADX regime + risk management *)
  { name = "P2: ADX<10,pos=10%";
    runner = fun c -> run_strategy c 10.0 15 0.3 0.3 fee 0.10 `All };
  { name = "P2: ADX<10,pos=25%,stop=15%";
    runner = fun c -> run_strategy c 10.0 15 0.3 0.3 fee 0.25 `All };

  (* Phase 3: Time-filtered *)
  { name = "P3: All-hours,ADX<10,pos=10%";
    runner = fun c -> run_strategy c 10.0 15 0.3 0.3 fee 0.10 `All };
  { name = "P3: London+WD,ADX<10,pos=10%,th=0.2";
    runner = fun c -> run_strategy c 10.0 15 0.2 0.3 fee 0.10 `LondonWeekday };
  { name = "P3: Late+WD,ADX<10,pos=10%,th=0.3";
    runner = fun c -> run_strategy c 10.0 15 0.3 0.3 fee 0.10 `LateWeekday };
  { name = "P3: US+WD,ADX<10,pos=10%,th=0.4";
    runner = fun c -> run_strategy c 10.0 15 0.4 0.3 fee 0.10 `USWeekday };
]

let () =
  let csv_path =
    try Sys.argv.(1) with _ -> "data/SOLUSDT.csv"
  in
  let all_candles = load_full_csv csv_path in
  Printf.eprintf "Loaded %d candles total\n%!" (Array.length all_candles);

  Printf.printf "\n=== Walk-Forward Validation + Sub-Period Analysis ===\n\n";

  List.iter (fun (strat : strategy) ->
    Printf.printf "Strategy: %s\n" strat.name;
    Printf.printf "%-20s %-6s %-6s %-7s %-9s  %s\n"
      "Period" "trades" "win%" "pf" "max_dd" "return";
    Printf.printf "%-20s %-6s %-6s %-7s %-9s  %s\n"
      "────────────────────" "──────" "──────" "───────" "─────────" "──────────";
    List.iter (fun (name, start_ts, end_ts) ->
      let slice = slice_by_timestamp all_candles start_ts end_ts in
      let count = Array.length slice in
      if count < 200 then
        Printf.printf "%-20s %-6s %-6s %-7s %-9s  %s\n"
          name "N/A" "N/A" "N/A" "N/A" "N/A"
      else
        let trades, wr, pf, mdd, ret = strat.runner slice in
        Printf.printf "%-20s %-6d %-5.1f%% %-7.2f %-7.2f%% %+.2f%%\n"
          name trades wr pf mdd ret
    ) periods;
    Printf.printf "\n"
  ) strategies;

  (* Summary: OOS vs IS comparison *)
  Printf.printf "=== Out-of-Sample vs In-Sample Comparison ===\n\n";
  Printf.printf "%-35s %-12s %-12s %-10s %-12s\n"
    "Strategy" "IS Return" "OOS Return" "Gap%" "OOS PF";
  Printf.printf "%-35s %-12s %-12s %-10s %-12s\n"
    "───────────────────────────────────" "────────────" "────────────" "──────────" "────────────";
  List.iter (fun (strat : strategy) ->
    let is_slice = slice_by_timestamp all_candles 1597125600 1711929600 in
    let oos_slice = slice_by_timestamp all_candles 1711929600 1778880780 in
    let _, _, _, _, is_ret = strat.runner is_slice in
    let _, _, oos_pf, _, oos_ret = strat.runner oos_slice in
    let gap = if is_ret <> 0.0 then (oos_ret -. is_ret) /. abs_float is_ret *. 100.0 else 0.0 in
    Printf.printf "%-35s %+10.2f%% %+10.2f%% %+.0f%%        %.2f\n"
      strat.name is_ret oos_ret gap oos_pf
  ) strategies
