(* Phase 3: Time-of-Day Pattern Analysis *)
(* Tests if hourly/daily/session patterns can improve the strategy *)

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
    let invest_amount = pf.cash *. (1.0 -. fee) *. position_size in
    let pos = invest_amount /. price in
    { pf with cash = pf.cash -. invest_amount; position = pos; entry_price = price }
  | Sell when pf.position > 0.0 ->
    let revenue = pf.position *. price *. (1.0 -. fee) in
    let cost_basis = pf.position *. pf.entry_price in
    let pnl = revenue -. cost_basis in
    if pnl > 0.0 then
      { pf with cash = pf.cash +. revenue; position = 0.0;
        trades = pf.trades + 1; wins = pf.wins + 1;
        total_won = pf.total_won +. pnl; entry_price = 0.0 }
    else
      { pf with cash = pf.cash +. revenue; position = 0.0;
        trades = pf.trades + 1;
        total_lost = pf.total_lost +. (-. pnl); entry_price = 0.0 }
  | _ -> pf

(* Time helpers *)
let hour_utc ts = (ts / 3600) mod 24
let day_of_week ts = (ts / 86400 + 4) mod 7  (* 1970-01-01 was Thursday *)
let is_weekend ts = let dow = day_of_week ts in dow = 5 || dow = 6

let session_name hour =
  if hour >= 0 && hour < 8 then "Asia"
  else if hour >= 8 && hour < 12 then "London"
  else if hour >= 12 && hour < 20 then "US"
  else "Late"

(* Backtest with time filter *)
let backtest_time_filter candles adx_threshold imb_ema_period buy_threshold
    sell_threshold fee position_size time_filter =
  let n = Array.length candles in
  let imbalances = Array.init n (fun i -> compute_imbalance candles.(i)) in
  let ema_imb = compute_ema imbalances imb_ema_period in
  let adx = compute_adx candles 14 in

  let pf = ref init_pf in
  let max_dd = ref 0.0 in

  for i = 200 to n - 1 do
    let price = candles.(i).close in
    let hour = hour_utc candles.(i).ts in
    let is_trending = adx.(i) > adx_threshold in

    let pass_filter = match time_filter with
      | `All -> true
      | `Asia -> hour >= 0 && hour < 8
      | `London -> hour >= 8 && hour < 12
      | `US -> hour >= 12 && hour < 20
      | `Late -> hour >= 20 && hour < 24
      | `Weekdays -> not (is_weekend candles.(i).ts)
      | `NoWeekends -> not (is_weekend candles.(i).ts)
      | `HighVolumeHours ->
        (* Hours 12-20 UTC = US session, typically highest volume *)
        hour >= 12 && hour < 20
      | `AsiaLondon -> hour >= 0 && hour < 12
      | `USLate -> hour >= 12 && hour < 24
    in

    if pass_filter && not is_trending then
      let signal =
        if ema_imb.(i) < -.buy_threshold then Buy
        else if ema_imb.(i) > sell_threshold then Sell
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

(* Analyze hourly returns and volume *)
let analyze_hourly_patterns candles =
  let n = Array.length candles in
  let hour_returns = Array.make 24 0.0 in
  let hour_counts = Array.make 24 0 in
  let hour_volumes = Array.make 24 0.0 in
  let hour_up_moves = Array.make 24 0.0 in
  let hour_down_moves = Array.make 24 0.0 in

  for i = 1 to n - 1 do
    let hour = hour_utc candles.(i).ts in
    let ret = (candles.(i).close -. candles.(i - 1).close) /. candles.(i - 1).close in
    hour_returns.(hour) <- hour_returns.(hour) +. ret;
    hour_counts.(hour) <- hour_counts.(hour) + 1;
    hour_volumes.(hour) <- hour_volumes.(hour) +. candles.(i).base_volume;
    if ret > 0.0 then hour_up_moves.(hour) <- hour_up_moves.(hour) +. ret
    else hour_down_moves.(hour) <- hour_down_moves.(hour) +. abs_float ret
  done;

  Printf.printf "\n=== Hourly Patterns (UTC) ===\n";
  Printf.printf "%-6s %-12s %-12s %-10s %-12s %-10s\n"
    "Hour" "Mean Return" "Volume" "Count" "Up/Down Ratio" "Session";
  for h = 0 to 23 do
    let mean_ret = if hour_counts.(h) > 0 then hour_returns.(h) /. float hour_counts.(h) *. 10000.0 else 0.0 in
    let avg_vol = if hour_counts.(h) > 0 then hour_volumes.(h) /. float hour_counts.(h) else 0.0 in
    let up_down = if hour_down_moves.(h) > 0.0 then hour_up_moves.(h) /. hour_down_moves.(h) else 0.0 in
    Printf.printf "%02d:00  %+.4f bps    %10.2f  %8d   %.4f        %s\n"
      h mean_ret avg_vol hour_counts.(h) up_down (session_name h)
  done

(* Analyze day-of-week patterns *)
let analyze_dow_patterns candles =
  let n = Array.length candles in
  let dow_returns = Array.make 7 0.0 in
  let dow_counts = Array.make 7 0 in
  let dow_volumes = Array.make 7 0.0 in

  for i = 1 to n - 1 do
    let dow = day_of_week candles.(i).ts in
    let ret = (candles.(i).close -. candles.(i - 1).close) /. candles.(i - 1).close in
    dow_returns.(dow) <- dow_returns.(dow) +. ret;
    dow_counts.(dow) <- dow_counts.(dow) + 1;
    dow_volumes.(dow) <- dow_volumes.(dow) +. candles.(i).base_volume
  done;

  let dow_names = ["Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat"; "Sun"] in
  Printf.printf "\n=== Day-of-Week Patterns ===\n";
  Printf.printf "%-6s %-12s %-12s %-10s\n" "Day" "Mean Return" "Volume" "Count";
  for d = 0 to 6 do
    let mean_ret = if dow_counts.(d) > 0 then dow_returns.(d) /. float dow_counts.(d) *. 10000.0 else 0.0 in
    let avg_vol = if dow_counts.(d) > 0 then dow_volumes.(d) /. float dow_counts.(d) else 0.0 in
    Printf.printf "%-6s %+.4f bps    %10.2f  %8d\n"
      (List.nth dow_names d) mean_ret avg_vol dow_counts.(d)
  done

let fee = 0.001

let () =
  let csv_path =
    try Sys.argv.(1) with _ -> "data/SOLUSDT.csv"
  in
  let candles = load_full_csv csv_path in
  Printf.eprintf "Loaded %d candles\n%!" (Array.length candles);

  (* Step 3.2: Descriptive Analysis *)
  analyze_hourly_patterns candles;
  analyze_dow_patterns candles;

  (* Step 3.3: Time-Filtered Backtests *)
  let position_size = 0.10 in
  let adx_threshold = 10.0 in
  let buy_threshold = 0.3 in
  let sell_threshold = 0.3 in

  Printf.printf "\n=== Time-Filtered Backtests (ADX 10, pos_size=0.10) ===\n";
  Printf.printf "%-15s %-6s %-6s %-7s %-9s  %s\n%!"
    "Filter" "trades" "win%" "pf" "max_dd" "return";
  List.iter (fun (name, filter) ->
    let trades, wr, pf, mdd, ret =
      backtest_time_filter candles adx_threshold 15 buy_threshold sell_threshold
        fee position_size filter in
    Printf.printf "%-15s %-6d %-5.1f%% %-7.2f %-7.2f%% %+.2f%%\n%!"
      name trades wr pf mdd ret
  ) [
    ("All", `All);
    ("Asia", `Asia);
    ("London", `London);
    ("US", `US);
    ("Late", `Late);
    ("Weekdays", `Weekdays);
    ("NoWeekends", `NoWeekends);
    ("HighVol(US)", `HighVolumeHours);
    ("Asia+London", `AsiaLondon);
    ("US+Late", `USLate);
  ];

  (* Step 3.4: Session-specific thresholds *)
  Printf.printf "\n=== Session-Specific Thresholds ===\n";
  Printf.printf "%-15s %-10s %-6s %-6s %-7s %-9s  %s\n%!"
    "Session" "buy_thresh" "trades" "win%" "pf" "max_dd" "return";
  List.iter (fun (name, filter) ->
    List.iter (fun bt ->
      let trades, wr, pf, mdd, ret =
        backtest_time_filter candles adx_threshold 15 bt sell_threshold
          fee position_size filter in
      Printf.printf "%-15s %-10.1f %-6d %-5.1f%% %-7.2f %-7.2f%% %+.2f%%\n%!"
        name bt trades wr pf mdd ret
    ) [0.2; 0.3; 0.4; 0.5]
  ) [
    ("Asia", `Asia);
    ("London", `London);
    ("US", `US);
    ("Late", `Late);
  ];

  (* Combined filters *)
  Printf.printf "\n=== Combined Filters (ADX 10, pos_size=0.10) ===\n";
  Printf.printf "%-25s %-6s %-6s %-7s %-9s  %s\n%!"
    "Filter" "trades" "win%" "pf" "max_dd" "return";

  (* Manual combined filter tests *)
  let test_combined name hour_filter dow_filter bt st =
    let n = Array.length candles in
    let imbalances = Array.init n (fun i -> compute_imbalance candles.(i)) in
    let ema_imb = compute_ema imbalances 15 in
    let adx = compute_adx candles 14 in
    let pf = ref init_pf in
    let max_dd = ref 0.0 in
    for i = 200 to n - 1 do
      let price = candles.(i).close in
      let hour = hour_utc candles.(i).ts in
      let is_trending = adx.(i) > adx_threshold in
      let pass_hour = match hour_filter with
        | `AllH -> true
        | `AsiaH -> hour >= 0 && hour < 8
        | `LondonH -> hour >= 8 && hour < 12
        | `USH -> hour >= 12 && hour < 20
        | `LateH -> hour >= 20 && hour < 24
      in
      let pass_dow = match dow_filter with
        | `AllD -> true
        | `WeekdaysD -> not (is_weekend candles.(i).ts)
        | `MonFriD -> let d = day_of_week candles.(i).ts in d = 0 || d = 4
      in
      if pass_hour && pass_dow && not is_trending then
        let signal =
          if ema_imb.(i) < -.bt then Buy
          else if ema_imb.(i) > st then Sell
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
    Printf.printf "%-25s %-6d %-5.1f%% %-7.2f %-7.2f%% %+.2f%%\n%!"
      name pf.trades win_rate profit_factor !max_dd total_return
  in

  List.iter (fun (name, hf, df) ->
    List.iter (fun bt ->
      test_combined name hf df bt 0.3
    ) [0.2; 0.3; 0.4]
  ) [
    ("London+Weekdays", `LondonH, `WeekdaysD);
    ("Late+Weekdays", `LateH, `WeekdaysD);
    ("US+Weekdays", `USH, `WeekdaysD);
    ("Asia+Weekdays", `AsiaH, `WeekdaysD);
    ("London+Mon/Fri", `LondonH, `MonFriD);
    ("Late+Mon/Fri", `LateH, `MonFriD);
  ]
