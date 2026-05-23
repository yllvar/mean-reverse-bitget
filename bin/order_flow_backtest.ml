(* Phase 1: Order Flow Imbalance Strategy Backtest *)
(* Tests if contrarian order flow signals can generate alpha *)

type candle_data = {
  close : float;
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
      | _ts :: _o :: _h :: _l :: close :: base_vol :: _quote_vol :: taker_buy :: _rest ->
        (try
          let close = float_of_string close in
          let base_vol = float_of_string base_vol in
          let taker_buy = float_of_string taker_buy in
          if base_vol > 0.0 then
            candles := { close; base_volume = base_vol; taker_buy_base = taker_buy } :: !candles
         with Failure _ -> ());
          loop ()
      | _ -> loop ()
    with End_of_file -> close_in ic
  in
  loop ();
  Array.of_list (List.rev !candles)

let compute_imbalance c =
  if c.base_volume <= 0.0 then 0.0
  else (c.taker_buy_base -. (c.base_volume -. c.taker_buy_base)) /. c.base_volume

type signal = Buy | Sell | Hold

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

(* EMA smoothing on imbalance *)
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

(* Volume median computation *)
let compute_volume_median candles =
  let vols = Array.map (fun c -> c.base_volume) candles in
  let sorted = Array.copy vols in
  Array.sort compare sorted;
  let n = Array.length sorted in
  if n mod 2 = 0 then (sorted.(n/2 - 1) +. sorted.(n/2)) /. 2.0
  else sorted.(n/2)

(* Strategy 1: Raw imbalance contrarian *)
(* If imbalance < -threshold (selling pressure) → BUY *)
(* If imbalance > +threshold (buying pressure) → SELL *)
let backtest_raw_imbalance candles threshold fee =
  let n = Array.length candles in
  let pf = ref init_pf in
  let max_dd = ref 0.0 in

  for i = 0 to n - 1 do
    let price = candles.(i).close in
    let imb = compute_imbalance candles.(i) in
    let signal =
      if imb < -.threshold then Buy
      else if imb > threshold then Sell
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
  (pf.trades, win_rate, profit_factor, !max_dd, total_return)

(* Strategy 2: EMA-smoothed imbalance contrarian *)
let backtest_ema_imbalance candles ema_period threshold fee =
  let n = Array.length candles in
  let imbalances = Array.init n (fun i -> compute_imbalance candles.(i)) in
  let ema = compute_ema imbalances ema_period in
  let pf = ref init_pf in
  let max_dd = ref 0.0 in

  for i = 0 to n - 1 do
    let price = candles.(i).close in
    let signal =
      if ema.(i) < -.threshold then Buy
      else if ema.(i) > threshold then Sell
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
  (pf.trades, win_rate, profit_factor, !max_dd, total_return)

(* Strategy 3: EMA imbalance with volume filter *)
let backtest_ema_volume candles ema_period threshold vol_mult fee =
  let n = Array.length candles in
  let imbalances = Array.init n (fun i -> compute_imbalance candles.(i)) in
  let ema = compute_ema imbalances ema_period in
  let vol_median = compute_volume_median candles in
  let vol_threshold = vol_median *. vol_mult in
  let pf = ref init_pf in
  let max_dd = ref 0.0 in

  for i = 0 to n - 1 do
    let price = candles.(i).close in
    let vol = candles.(i).base_volume in
    let signal =
      if vol < vol_threshold then Hold  (* Skip low volume candles *)
      else if ema.(i) < -.threshold then Buy
      else if ema.(i) > threshold then Sell
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
  (pf.trades, win_rate, profit_factor, !max_dd, total_return)

(* Strategy 4: EMA imbalance + SMA trend filter *)
let backtest_ema_sma candles ema_period threshold sma_period fee =
  let n = Array.length candles in
  let imbalances = Array.init n (fun i -> compute_imbalance candles.(i)) in
  let ema = compute_ema imbalances ema_period in
  let pf = ref init_pf in
  let max_dd = ref 0.0 in
  let price_window = Array.make sma_period 0.0 in
  let price_idx = ref 0 in
  let price_sum = ref 0.0 in

  (* Initialize price window *)
  for i = 0 to sma_period - 1 do
    price_window.(i) <- candles.(i).close;
    price_sum := !price_sum +. candles.(i).close
  done;

  for i = sma_period to n - 1 do
    let price = candles.(i).close in
    let sma = !price_sum /. float sma_period in

    (* Update price window *)
    let old_price = price_window.(!price_idx) in
    price_window.(!price_idx) <- price;
    price_sum := !price_sum -. old_price +. price;
    price_idx := (!price_idx + 1) mod sma_period;

    let signal =
      if ema.(i) < -.threshold then
        if price > sma then Buy else Hold  (* Only buy if above SMA *)
      else if ema.(i) > threshold then Sell
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
  (pf.trades, win_rate, profit_factor, !max_dd, total_return)

let fee = 0.001

let () =
  let csv_path =
    try Sys.argv.(1) with _ -> "data/SOLUSDT.csv"
  in
  let candles = load_full_csv csv_path in
  let n = Array.length candles in
  Printf.eprintf "Loaded %d candles with volume data\n%!" n;

  (* Strategy 1: Raw imbalance *)
  Printf.printf "\n=== Strategy 1: Raw Imbalance Contrarian ===\n";
  Printf.printf "%-10s %-6s %-6s %-7s %-9s  %s\n%!"
    "threshold" "trades" "win%" "pf" "max_dd" "return";
  List.iter (fun t ->
    let trades, wr, pf, mdd, ret = backtest_raw_imbalance candles t fee in
    Printf.printf "%-10.1f %-6d %-5.1f%% %-7.2f %-7.2f%% %+.2f%%\n%!"
      t trades wr pf mdd ret
  ) [0.1; 0.2; 0.3; 0.5; 0.7];

  (* Strategy 2: EMA-smoothed imbalance *)
  Printf.printf "\n=== Strategy 2: EMA-Smoothed Imbalance ===\n";
  Printf.printf "%-6s %-10s %-6s %-6s %-7s %-9s  %s\n%!"
    "ema" "threshold" "trades" "win%" "pf" "max_dd" "return";
  List.iter (fun ema ->
    List.iter (fun t ->
      let trades, wr, pf, mdd, ret = backtest_ema_imbalance candles ema t fee in
      Printf.printf "%-6d %-10.1f %-6d %-5.1f%% %-7.2f %-7.2f%% %+.2f%%\n%!"
        ema t trades wr pf mdd ret
    ) [0.1; 0.2; 0.3; 0.5]
  ) [5; 15; 50; 200];

  (* Strategy 3: EMA + Volume filter *)
  Printf.printf "\n=== Strategy 3: EMA + Volume Filter ===\n";
  Printf.printf "%-6s %-10s %-8s %-6s %-6s %-7s %-9s  %s\n%!"
    "ema" "threshold" "vol_mult" "trades" "win%" "pf" "max_dd" "return";
  List.iter (fun ema ->
    List.iter (fun t ->
      List.iter (fun vm ->
        let trades, wr, pf, mdd, ret = backtest_ema_volume candles ema t vm fee in
        Printf.printf "%-6d %-10.1f %-8.1f %-6d %-5.1f%% %-7.2f %-7.2f%% %+.2f%%\n%!"
          ema t vm trades wr pf mdd ret
      ) [1.0; 2.0]
    ) [0.2; 0.3]
  ) [15; 50];

  (* Strategy 4: EMA + SMA trend filter *)
  Printf.printf "\n=== Strategy 4: EMA + SMA Trend Filter ===\n";
  Printf.printf "%-6s %-10s %-6s %-6s %-6s %-7s %-9s  %s\n%!"
    "ema" "threshold" "sma" "trades" "win%" "pf" "max_dd" "return";
  List.iter (fun ema ->
    List.iter (fun t ->
      List.iter (fun sma ->
        let trades, wr, pf, mdd, ret = backtest_ema_sma candles ema t sma fee in
        Printf.printf "%-6d %-10.1f %-6d %-6d %-5.1f%% %-7.2f %-7.2f%% %+.2f%%\n%!"
          ema t sma trades wr pf mdd ret
      ) [200; 500]
    ) [0.2; 0.3; 0.5]
  ) [15; 50]
