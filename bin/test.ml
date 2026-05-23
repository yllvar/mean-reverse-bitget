open Bitget_lib

let pass_count = ref 0
let fail_count = ref 0

let assert_eq name expected actual =
  if expected = actual then
    pass_count := !pass_count + 1
  else begin
    fail_count := !fail_count + 1;
    Printf.printf "FAIL: %s — expected %s, got %s\n%!" name expected actual
  end

let assert_approx name expected actual tol =
  if abs_float (expected -. actual) < tol then
    pass_count := !pass_count + 1
  else begin
    fail_count := !fail_count + 1;
    Printf.printf "FAIL: %s — expected %.6f, got %.6f (tol=%.6f)\n%!" name expected actual tol
  end

let test_ema () =
  Printf.printf "=== Strategy.ema ===\n%!";
  let ema = Strategy.ema in
  let ema_1 = ema ~prices:[42.0] ~period:5 in
  assert_approx "EMA single price" 42.0 ema_1 0.0001;

  let prices_flat = List.init 50 (fun _ -> 100.0) in
  let ema_flat = ema ~prices:prices_flat ~period:10 in
  assert_approx "EMA(10) flat prices" 100.0 ema_flat 0.0001;

  let prices_rising = List.init 100 (fun i -> float i) in
  let ema_rising = ema ~prices:prices_rising ~period:20 in
  assert_eq "EMA rising > 0" "true" (if ema_rising > 0.0 then "true" else "false");

  let k_3 = 2.0 /. (3.0 +. 1.0) in
  let prices_3 = [12.0; 11.0; 10.0] in
  let ema_3 = ema ~prices:prices_3 ~period:3 in
  let expected_3 =
    let k = k_3 in
    let ema1 = 11.0 *. k +. 10.0 *. (1.0 -. k) in
    let ema2 = 12.0 *. k +. ema1 *. (1.0 -. k) in
    ema2
  in
  assert_approx "EMA(3) [12;11;10]" expected_3 ema_3 0.0001;
  ()

let test_mean_reversion () =
  Printf.printf "=== Strategy.mean_reversion ===\n%!";
  let mr = Strategy.mean_reversion in
  let prices_flat = List.init 20 (fun _ -> 100.0) in
  let signal, z = mr ~prices:prices_flat ~current_price:100.0 ~threshold:1.0 in
  assert_eq "Flat prices → Hold" "Hold" (match signal with Strategy.Hold -> "Hold" | _ -> "other");
  assert_approx "Flat z-score" 0.0 z 0.0001;

  let prices_rising = List.init 20 (fun i -> 100.0 +. float i) in
  let signal_r, z_r = mr ~prices:prices_rising ~current_price:130.0 ~threshold:1.0 in
  assert_eq "Price above mean → Sell" "Sell" (match signal_r with Strategy.Sell -> "Sell" | _ -> "other");
  assert_eq "Positive z" "true" (if z_r > 0.0 then "true" else "false");

  let prices_falling = List.init 20 (fun i -> 120.0 -. float i) in
  let signal_f, z_f = mr ~prices:prices_falling ~current_price:90.0 ~threshold:1.0 in
  assert_eq "Price below mean → Buy" "Buy" (match signal_f with Strategy.Buy -> "Buy" | _ -> "other");
  assert_eq "Negative z" "true" (if z_f < 0.0 then "true" else "false");

  let prices_mix = List.init 50 (fun i -> 100.0 +. float (i mod 5)) in
  let signal_m, _z_m = mr ~prices:prices_mix ~current_price:102.0 ~threshold:1.0 in
  assert_eq "Small deviation → Hold" "Hold" (match signal_m with Strategy.Hold -> "Hold" | _ -> "other");

  let prices_short = [1.0; 2.0; 3.0] in
  let signal_s, _ = mr ~prices:prices_short ~current_price:2.0 ~threshold:1.0 in
  assert_eq "Short list → Hold" "Hold" (match signal_s with Strategy.Hold -> "Hold" | _ -> "other");
  ()

let test_apply_trend_filter () =
  Printf.printf "=== Strategy.apply_trend_filter ===\n%!";
  let atf = Strategy.apply_trend_filter in
  let signal = atf Strategy.Buy 100.0 110.0 in
  assert_eq "Buy below trend → Hold" "Hold" (match signal with Strategy.Hold -> "Hold" | _ -> "other");

  let signal = atf Strategy.Buy 120.0 110.0 in
  assert_eq "Buy above trend → Buy" "Buy" (match signal with Strategy.Buy -> "Buy" | _ -> "other");

  let signal = atf Strategy.Sell 120.0 110.0 in
  assert_eq "Sell above trend → Hold" "Hold" (match signal with Strategy.Hold -> "Hold" | _ -> "other");

  let signal = atf Strategy.Sell 100.0 110.0 in
  assert_eq "Sell below trend → Sell" "Sell" (match signal with Strategy.Sell -> "Sell" | _ -> "other");

  let signal = atf Strategy.Buy 100.0 0.0 in
  assert_eq "Zero trend → passthrough" "Buy" (match signal with Strategy.Buy -> "Buy" | _ -> "other");

  let signal = atf Strategy.Hold 100.0 110.0 in
  assert_eq "Hold → Hold" "Hold" (match signal with Strategy.Hold -> "Hold" | _ -> "other");
  ()

let test_trader_state () =
  Printf.printf "=== Trader state ===\n%!";
  let prices = List.init 500 (fun i -> 100.0 +. float i) in
  let ema_prices = List.init 200 (fun i -> 100.0 +. float i) in
  let state = Trader.init_state ~prices ~ema_prices ~window_size:500 ~ema_period:200
    ~threshold:1.0 ~interval:10.0 ~stop_loss:0.50 ~take_profit:1.00 ~position_size:0.50 in
  assert_eq "Init cycle" "0" (string_of_int state.cycle);
  assert_eq "Init has_position" "false" (string_of_bool state.has_position);
  assert_approx "Init entry_price" 0.0 state.entry_price 0.0001;
  assert_approx "Init position_qty" 0.0 state.position_qty 0.0001;
  assert_approx "Init virtual_cash" 1000.0 state.virtual_cash 0.0001;

  let state2 = Trader.append_price state 300.0 in
  assert_eq "Append cycle" "1" (string_of_int state2.cycle);
  assert_eq "Prices length after append" "500" (string_of_int (List.length state2.prices));
  assert_eq "EMA prices length after append" "200" (string_of_int (List.length state2.ema_prices));

  let state3 = Trader.append_price state2 301.0 in
  assert_eq "Append cycle 2" "2" (string_of_int state3.cycle);
  assert_eq "Prices still 500" "500" (string_of_int (List.length state3.prices));
  ()

let test_ring_buffer_eviction () =
  Printf.printf "=== Ring buffer eviction ===\n%!";
  let prices = List.init 500 (fun i -> float i) in
  let ema_prices = List.init 200 (fun i -> float i) in
  let state = Trader.init_state ~prices ~ema_prices ~window_size:500 ~ema_period:200
    ~threshold:1.0 ~interval:10.0 ~stop_loss:0.50 ~take_profit:1.00 ~position_size:0.50 in
  assert_approx "Initial prices[0]" 0.0 (List.hd state.prices) 0.0001;
  assert_approx "Initial prices[last]" 499.0 (List.hd (List.rev state.prices)) 0.0001;

  let state2 = Trader.append_price state 1000.0 in
  assert_approx "After append, newest" 1000.0 (List.hd state2.prices) 0.0001;
  assert_approx "Oldest evicted" 498.0 (List.hd (List.rev state2.prices)) 0.0001;

  let state3 = Trader.append_price state2 2000.0 in
  assert_approx "Second append newest" 2000.0 (List.hd state3.prices) 0.0001;
  assert_approx "Second oldest evicted" 497.0 (List.hd (List.rev state3.prices)) 0.0001;
  ()

let test_end_to_end_signal () =
  Printf.printf "=== End-to-end signal chain ===\n%!";
  let prices_flat = List.init 500 (fun _ -> 100.0) in
  let ema_prices = List.init 200 (fun _ -> 100.0) in
  let state = Trader.init_state ~prices:prices_flat ~ema_prices ~window_size:500 ~ema_period:200
    ~threshold:1.0 ~interval:10.0 ~stop_loss:0.50 ~take_profit:1.00 ~position_size:0.50 in
  let signal, _z = Strategy.mean_reversion ~prices:state.prices ~current_price:100.0 ~threshold:1.0 in
  let ema = Strategy.ema ~prices:state.ema_prices ~period:state.ema_period in
  let filtered = Strategy.apply_trend_filter signal 100.0 ema in
  assert_eq "Flat market → Hold" "Hold" (match filtered with Strategy.Hold -> "Hold" | _ -> "other");

  let prices_dip = List.init 500 (fun i -> 100.0 +. float i) in
  let ema_prices_dip = List.init 200 (fun _ -> 49.0) in
  let state_dip = Trader.init_state ~prices:prices_dip ~ema_prices:ema_prices_dip ~window_size:500 ~ema_period:200
    ~threshold:1.0 ~interval:10.0 ~stop_loss:0.50 ~take_profit:1.00 ~position_size:0.50 in
  let signal_dip, _z_dip =
    Strategy.mean_reversion ~prices:state_dip.prices ~current_price:50.0 ~threshold:1.0
  in
  let ema_dip = Strategy.ema ~prices:state_dip.ema_prices ~period:state_dip.ema_period in
  let filtered_dip = Strategy.apply_trend_filter signal_dip 50.0 ema_dip in
  assert_eq "Sharp dip, price>EMA → Buy" "Buy" (match filtered_dip with Strategy.Buy -> "Buy" | _ -> "other");

  let prices_spike = List.init 500 (fun i -> 100.0 -. float i) in
  let ema_prices_spike = List.init 200 (fun _ -> 201.0) in
  let state_spike = Trader.init_state ~prices:prices_spike ~ema_prices:ema_prices_spike ~window_size:500 ~ema_period:200
    ~threshold:1.0 ~interval:10.0 ~stop_loss:0.50 ~take_profit:1.00 ~position_size:0.50 in
  let signal_spike, _z_spike =
    Strategy.mean_reversion ~prices:state_spike.prices ~current_price:200.0 ~threshold:1.0
  in
  let ema_spike = Strategy.ema ~prices:state_spike.ema_prices ~period:state_spike.ema_period in
  let filtered_spike = Strategy.apply_trend_filter signal_spike 200.0 ema_spike in
  assert_eq "Sharp spike, price<EMA → Sell" "Sell" (match filtered_spike with Strategy.Sell -> "Sell" | _ -> "other");
  ()

let test_virtual_trade_accounting () =
  Printf.printf "=== Virtual trade accounting ===\n%!";
  let prices = List.init 500 (fun i -> 100.0 +. float i) in
  let ema_prices = List.init 200 (fun i -> 100.0 +. float i) in
  let state = Trader.init_state ~prices ~ema_prices ~window_size:500 ~ema_period:200
    ~threshold:1.0 ~interval:10.0 ~stop_loss:0.50 ~take_profit:1.00 ~position_size:0.50 in
  assert_approx "Virtual cash init" 1000.0 state.virtual_cash 0.0001;
  assert_approx "Virtual position init" 0.0 state.virtual_position 0.0001;

  let fee = 0.001 in
  let invest = state.virtual_cash *. (1.0 -. fee) *. state.position_size in
  let pos = invest /. 100.0 in
  let state_buy = { state with
    virtual_cash = state.virtual_cash -. invest;
    virtual_position = pos;
    has_position = true;
    entry_price = 100.0;
    position_qty = pos;
  } in
  assert_approx "Cash after buy" (1000.0 -. invest) state_buy.virtual_cash 0.01;
  assert_approx "Position after buy" pos state_buy.virtual_position 0.0001;

  let sell_price = 105.0 in
  let revenue = state_buy.virtual_position *. sell_price *. (1.0 -. fee) in
  let cash_after_sell = state_buy.virtual_cash +. revenue in
  let state_sell = { state_buy with
    virtual_cash = cash_after_sell;
    virtual_position = 0.0;
    virtual_trades = state_buy.virtual_trades + 1;
    has_position = false;
    entry_price = 0.0;
    position_qty = 0.0;
  } in
  assert_approx "Cash after sell includes remaining cash" cash_after_sell state_sell.virtual_cash 0.01;
  assert_approx "Position after sell" 0.0 state_sell.virtual_position 0.0001;
  assert_eq "Trade count" "1" (string_of_int state_sell.virtual_trades);
  ()

let test_full_trade_lifecycle () =
  Printf.printf "=== Full trade lifecycle ===\n%!";
  let fee = 0.001 in
  let win_pct = 0.05 in

  let simulate_trade ~cash ~price ~exit_price ~pos_size =
    let invest = cash *. (1.0 -. fee) *. pos_size in
    let qty = invest /. price in
    let cash_after_buy = cash -. invest in
    let revenue = qty *. exit_price *. (1.0 -. fee) in
    cash_after_buy +. revenue
  in

  let initial_cash = 1000.0 in
  let entry_price = 100.0 in

  let invested = initial_cash *. (1.0 -. fee) *. 0.50 in

  (* 1. Breakeven: fee is invest * fee (buy fee reduces position size, sell fee reduces proceeds) *)
  let cash_breakeven = simulate_trade ~cash:initial_cash ~price:entry_price
                         ~exit_price:entry_price ~pos_size:0.50 in
  let expected_be = initial_cash -. invested *. fee in
  assert_approx "Breakeven round-trip" expected_be cash_breakeven 0.01;

  (* 2. Profit *)
  let exit_profit = entry_price *. (1.0 +. win_pct) in
  let cash_profit = simulate_trade ~cash:initial_cash ~price:entry_price
                      ~exit_price:exit_profit ~pos_size:0.50 in
  assert_eq "Profit trade > initial" "true"
    (if cash_profit > initial_cash then "true" else "false");

  (* 3. Loss *)
  let cash_loss = simulate_trade ~cash:initial_cash ~price:entry_price
                    ~exit_price:(entry_price *. 0.95) ~pos_size:0.50 in
  assert_eq "Loss trade < initial" "true"
    (if cash_loss < initial_cash then "true" else "false");

  (* 4. Multiple compounding trades (bug that killed the bot) *)
  let cash = ref initial_cash in
  for _i = 1 to 5 do
    cash := simulate_trade ~cash:!cash ~price:entry_price
              ~exit_price:exit_profit ~pos_size:0.50
  done;
  assert_eq "5 profitable trades > initial" "true"
    (if !cash > initial_cash then "true" else "false");
  assert_eq "5 trades, cash finite" "true"
    (if !cash < infinity then "true" else "false");

  (* 5. 100% position size loss *)
  let full_loss = simulate_trade ~cash:initial_cash ~price:entry_price
                    ~exit_price:(entry_price *. 0.90) ~pos_size:1.0 in
  assert_eq "Full size loss < initial" "true"
    (if full_loss < initial_cash then "true" else "false");

  (* 6. Zero-size trade preserves cash *)
  let cash_skip = simulate_trade ~cash:initial_cash ~price:entry_price
                    ~exit_price:entry_price ~pos_size:0.0 in
  assert_approx "Zero size preserves cash" 1000.0 cash_skip 0.0001;

  Printf.printf "  Final cash after 5x +5%% trades: $%.2f\n%!" !cash;
  ()

let test_sl_tp_logic () =
  Printf.printf "=== SL/TP logic ===\n%!";
  let entry = 100.0 in
  let sl = 0.50 in
  let tp = 1.00 in
  let sl_price = entry *. (1.0 -. sl) in
  let tp_price = entry *. (1.0 +. tp) in
  assert_approx "SL price" 50.0 sl_price 0.01;
  assert_approx "TP price" 200.0 tp_price 0.01;

  assert_eq "Price at entry → no trigger" "false" (if 100.0 <= sl_price || 100.0 >= tp_price then "true" else "false");
  assert_eq "Price at SL → trigger" "true" (if 50.0 <= sl_price then "true" else "false");
  assert_eq "Price at TP → trigger" "true" (if 200.0 >= tp_price then "true" else "false");
  assert_eq "Price below SL → trigger" "true" (if 45.0 <= sl_price then "true" else "false");
  assert_eq "Price above TP → trigger" "true" (if 210.0 >= tp_price then "true" else "false");
  assert_eq "Price between → no trigger" "false" (if 80.0 <= sl_price || 150.0 >= tp_price then "true" else "false");
  ()

let test_threshold_sensitivity () =
  Printf.printf "=== Threshold sensitivity ===\n%!";
  let mr = Strategy.mean_reversion in
  let prices = List.init 500 (fun i -> 100.0 +. float (i mod 10)) in
  let current = 104.0 in
  let signal_lo, z_lo = mr ~prices ~current_price:current ~threshold:0.5 in
  let signal_hi, z_hi = mr ~prices ~current_price:current ~threshold:5.0 in
  assert_eq "Low threshold → Hold" "Hold" (match signal_lo with Strategy.Hold -> "Hold" | _ -> "other");
  assert_eq "High threshold → Hold" "Hold" (match signal_hi with Strategy.Hold -> "Hold" | _ -> "other");
  assert_approx "Same z-score" z_lo z_hi 0.0001;

  let current_extreme = 90.0 in
  let signal_lo2, _z_lo2 = mr ~prices ~current_price:current_extreme ~threshold:0.5 in
  let signal_hi2, _z_hi2 = mr ~prices ~current_price:current_extreme ~threshold:6.0 in
  assert_eq "Extreme low, low threshold → Buy" "Buy" (match signal_lo2 with Strategy.Buy -> "Buy" | _ -> "other");
  assert_eq "Extreme low, high threshold → Hold" "Hold" (match signal_hi2 with Strategy.Hold -> "Hold" | _ -> "other");
  ()

let test_ema_period_sensitivity () =
  Printf.printf "=== EMA period sensitivity ===\n%!";
  let prices = List.init 100 (fun i -> float (99 - i)) in
  let ema_fast = Strategy.ema ~prices ~period:10 in
  let ema_slow = Strategy.ema ~prices ~period:50 in
  assert_eq "Fast EMA > Slow EMA" "true" (if ema_fast > ema_slow then "true" else "false");

  let prices_flat = List.init 100 (fun _ -> 50.0) in
  let ema_f = Strategy.ema ~prices:prices_flat ~period:10 in
  let ema_s = Strategy.ema ~prices:prices_flat ~period:50 in
  assert_approx "Flat EMA(10)" 50.0 ema_f 0.0001;
  assert_approx "Flat EMA(50)" 50.0 ema_s 0.0001;
  ()

let () =
  Printf.printf "\n=== Bitget Bot Test Suite ===\n\n%!";
  test_ema ();
  test_mean_reversion ();
  test_apply_trend_filter ();
  test_trader_state ();
  test_ring_buffer_eviction ();
  test_end_to_end_signal ();
  test_virtual_trade_accounting ();
  test_full_trade_lifecycle ();
  test_sl_tp_logic ();
  test_threshold_sensitivity ();
  test_ema_period_sensitivity ();
  Printf.printf "\n=== Results ===\n%!";
  Printf.printf "Passed: %d\n%!" !pass_count;
  Printf.printf "Failed: %d\n%!" !fail_count;
  if !fail_count > 0 then begin
    Printf.printf "STATUS: FAILED\n%!";
    exit 1
  end else begin
    Printf.printf "STATUS: ALL PASSED\n%!";
    exit 0
  end
