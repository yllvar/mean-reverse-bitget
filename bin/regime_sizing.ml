(* Phase 2.6: Position Sizing + Time-Based Exits *)
(* Tests if partial position sizing and time exits reduce drawdown *)

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
  entry_bar : int;
  trades : int;
  wins : int;
  total_won : float;
  total_lost : float;
  equity_peak : float;
}

let init_pf = {
  cash = 1000.0; position = 0.0; entry_price = 0.0; entry_bar = 0;
  trades = 0; wins = 0;
  total_won = 0.0; total_lost = 0.0;
  equity_peak = 1000.0;
}

let equity pf price = pf.cash +. pf.position *. price

let apply_trade pf signal price fee position_size max_hold_bars current_bar stop_loss_pct =
  match signal with
  | Buy when pf.position = 0.0 ->
    let invest_amount = pf.cash *. (1.0 -. fee) *. position_size in
    let pos = invest_amount /. price in
    { pf with cash = pf.cash -. invest_amount; position = pos;
      entry_price = price; entry_bar = current_bar }
  | Sell when pf.position > 0.0 ->
    let revenue = pf.position *. price *. (1.0 -. fee) in
    let cost_basis = pf.position *. pf.entry_price in
    let pnl = revenue -. cost_basis in
    if pnl > 0.0 then
      { pf with cash = pf.cash +. revenue; position = 0.0;
        trades = pf.trades + 1; wins = pf.wins + 1;
        total_won = pf.total_won +. pnl;
        entry_price = 0.0; entry_bar = 0 }
    else
      { pf with cash = pf.cash +. revenue; position = 0.0;
        trades = pf.trades + 1;
        total_lost = pf.total_lost +. (-. pnl);
        entry_price = 0.0; entry_bar = 0 }
  | _ ->
    (* Check time-based exit and stop loss *)
    if pf.position > 0.0 then
      let held_bars = current_bar - pf.entry_bar in
      let drop_from_entry = (pf.entry_price -. price) /. pf.entry_price in
      let time_exit = max_hold_bars > 0 && held_bars >= max_hold_bars in
      let stop_exit = stop_loss_pct > 0.0 && drop_from_entry >= stop_loss_pct in
      if time_exit || stop_exit then
        let revenue = pf.position *. price *. (1.0 -. fee) in
        let cost_basis = pf.position *. pf.entry_price in
        let pnl = revenue -. cost_basis in
        if pnl > 0.0 then
          { pf with cash = pf.cash +. revenue; position = 0.0;
            trades = pf.trades + 1; wins = pf.wins + 1;
            total_won = pf.total_won +. pnl;
            entry_price = 0.0; entry_bar = 0 }
        else
          { pf with cash = pf.cash +. revenue; position = 0.0;
            trades = pf.trades + 1;
            total_lost = pf.total_lost +. (-. pnl);
            entry_price = 0.0; entry_bar = 0 }
      else pf
    else pf

let backtest candles adx_threshold imb_ema_period buy_threshold sell_threshold
    fee position_size max_hold_bars stop_loss_pct =
  let n = Array.length candles in
  let imbalances = Array.init n (fun i -> compute_imbalance candles.(i)) in
  let ema_imb = compute_ema imbalances imb_ema_period in
  let adx = compute_adx candles 14 in

  let pf = ref init_pf in
  let max_dd = ref 0.0 in

  for i = 200 to n - 1 do
    let price = candles.(i).close in
    let is_trending = adx.(i) > adx_threshold in

    let signal =
      if is_trending then Hold
      else if ema_imb.(i) < -.buy_threshold then Buy
      else if ema_imb.(i) > sell_threshold then Sell
      else Hold
    in

    pf := apply_trade !pf signal price fee position_size max_hold_bars i stop_loss_pct;
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

let fee = 0.001

let () =
  let csv_path =
    try Sys.argv.(1) with _ -> "data/SOLUSDT.csv"
  in
  let candles = load_full_csv csv_path in
  Printf.eprintf "Loaded %d candles\n%!" (Array.length candles);

  (* Position sizing tests *)
  Printf.printf "\n=== ADX 10 Range-Only + Position Sizing (no time exit, no stop) ===\n";
  Printf.printf "%-8s %-6s %-6s %-7s %-9s  %s\n%!"
    "pos_size" "trades" "win%" "pf" "max_dd" "return";
  List.iter (fun ps ->
    let trades, wr, pf, mdd, ret =
      backtest candles 10.0 15 0.3 0.3 fee ps 0 0.0 in
    Printf.printf "%-8.2f %-6d %-5.1f%% %-7.2f %-7.2f%% %+.2f%%\n%!"
      ps trades wr pf mdd ret
  ) [0.10; 0.20; 0.30; 0.40; 0.50; 0.75; 1.00];

  (* Position sizing + time exit *)
  Printf.printf "\n=== ADX 10 Range-Only + Position Size + Time Exit ===\n";
  Printf.printf "%-8s %-8s %-6s %-6s %-7s %-9s  %s\n%!"
    "pos_size" "max_bars" "trades" "win%" "pf" "max_dd" "return";
  List.iter (fun ps ->
    List.iter (fun mb ->
      let trades, wr, pf, mdd, ret =
        backtest candles 10.0 15 0.3 0.3 fee ps mb 0.0 in
      Printf.printf "%-8.2f %-8d %-6d %-5.1f%% %-7.2f %-7.2f%% %+.2f%%\n%!"
        ps mb trades wr pf mdd ret
    ) [60; 120; 240; 480; 1440]
  ) [0.25; 0.50; 0.75];

  (* Position sizing + stop loss *)
  Printf.printf "\n=== ADX 10 Range-Only + Position Size + Stop Loss ===\n";
  Printf.printf "%-8s %-8s %-6s %-6s %-7s %-9s  %s\n%!"
    "pos_size" "stop%" "trades" "win%" "pf" "max_dd" "return";
  List.iter (fun ps ->
    List.iter (fun sl ->
      let trades, wr, pf, mdd, ret =
        backtest candles 10.0 15 0.3 0.3 fee ps 0 sl in
      Printf.printf "%-8.2f %-8.2f %-6d %-5.1f%% %-7.2f %-7.2f%% %+.2f%%\n%!"
        ps (sl *. 100.0) trades wr pf mdd ret
    ) [0.05; 0.10; 0.15]
  ) [0.25; 0.50; 0.75];

  (* Best combos: position size + time exit + stop *)
  Printf.printf "\n=== ADX 10 Range-Only + Pos Size + Time Exit + Stop ===\n";
  Printf.printf "%-6s %-6s %-6s %-6s %-6s %-7s %-9s  %s\n%!"
    "pos" "bars" "stop%" "trades" "win%" "pf" "max_dd" "return";
  List.iter (fun ps ->
    List.iter (fun mb ->
      List.iter (fun sl ->
        let trades, wr, pf, mdd, ret =
          backtest candles 10.0 15 0.3 0.3 fee ps mb sl in
        Printf.printf "%-6.2f %-6d %-6.2f %-6d %-5.1f%% %-7.2f %-7.2f%% %+.2f%%\n%!"
          ps mb (sl *. 100.0) trades wr pf mdd ret
      ) [0.05; 0.10]
    ) [120; 240; 480]
  ) [0.25; 0.50];

  (* Kelly-inspired: vary position size by ADX level *)
  Printf.printf "\n=== ADX Threshold Sweep + Best Risk Params (ps=0.50, bars=240, stop=10%%) ===\n";
  Printf.printf "%-12s %-6s %-6s %-7s %-9s  %s\n%!"
    "adx_thresh" "trades" "win%" "pf" "max_dd" "return";
  List.iter (fun t ->
    let trades, wr, pf, mdd, ret =
      backtest candles t 15 0.3 0.3 fee 0.50 240 0.10 in
    Printf.printf "%-12.1f %-6d %-5.1f%% %-7.2f %-7.2f%% %+.2f%%\n%!"
      t trades wr pf mdd ret
  ) [8.0; 10.0; 12.0; 14.0; 15.0; 18.0; 20.0; 25.0; 30.0]
