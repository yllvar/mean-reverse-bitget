(* Phase 1: Order Flow Imbalance Analysis *)
(* Explores taker buy/sell volume relationship with future price returns *)

type candle_data = {
  ts : int;
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
      | ts :: _o :: _h :: _l :: close :: base_vol :: _quote_vol :: taker_buy :: _rest ->
        (try
          let ts = int_of_string ts in
          let close = float_of_string close in
          let base_vol = float_of_string base_vol in
          let taker_buy = float_of_string taker_buy in
          if base_vol > 0.0 then
            candles := { ts; close; base_volume = base_vol; taker_buy_base = taker_buy } :: !candles
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

let compute_log_return p1 p2 =
  if p1 <= 0.0 || p2 <= 0.0 then 0.0
  else log (p2 /. p1)

let () =
  let csv_path =
    try Sys.argv.(1) with _ -> "data/SOLUSDT.csv"
  in
  let candles = load_full_csv csv_path in
  let n = Array.length candles in
  Printf.eprintf "Loaded %d candles with volume data\n%!" n;

  (* Compute imbalance for all candles *)
  let imbalances = Array.make n 0.0 in
  for i = 0 to n - 1 do
    imbalances.(i) <- compute_imbalance candles.(i)
  done;

  (* Imbalance distribution statistics *)
  let imb_sum = ref 0.0 in
  let imb_min = ref 1.0 in
  let imb_max = ref (-1.0) in
  let imb_sq_sum = ref 0.0 in
  let pos_count = ref 0 in
  let neg_count = ref 0 in
  let zero_count = ref 0 in

  for i = 0 to n - 1 do
    let imb = imbalances.(i) in
    imb_sum := !imb_sum +. imb;
    imb_sq_sum := !imb_sq_sum +. imb *. imb;
    if imb < !imb_min then imb_min := imb;
    if imb > !imb_max then imb_max := imb;
    if imb > 0.0 then incr pos_count
    else if imb < 0.0 then incr neg_count
    else incr zero_count
  done;

  let imb_mean = !imb_sum /. float n in
  let imb_var = !imb_sq_sum /. float n -. imb_mean *. imb_mean in
  let imb_std = sqrt imb_var in

  Printf.printf "=== Order Flow Imbalance Distribution ===\n";
  Printf.printf "Count: %d\n" n;
  Printf.printf "Mean: %.6f\n" imb_mean;
  Printf.printf "Std:  %.6f\n" imb_std;
  Printf.printf "Min:  %.6f\n" !imb_min;
  Printf.printf "Max:  %.6f\n" !imb_max;
  Printf.printf "Positive: %d (%.1f%%)\n" !pos_count (float !pos_count /. float n *. 100.0);
  Printf.printf "Negative: %d (%.1f%%)\n" !neg_count (float !neg_count /. float n *. 100.0);
  Printf.printf "Zero:     %d (%.1f%%)\n\n" !zero_count (float !zero_count /. float n *. 100.0);

  (* Cross-correlation with future returns *)
  let lags = [1; 5; 15; 60; 120; 240; 480; 1440] in
  Printf.printf "=== Cross-Correlation: Imbalance vs Future Returns ===\n";
  Printf.printf "%-8s %-15s %-15s %-15s\n" "Lag" "Correlation" "Mean Return(+imb)" "Mean Return(-imb)";

  List.iter (fun lag ->
    if lag < n then
      let sum_xy = ref 0.0 in
      let sum_x = ref 0.0 in
      let sum_y = ref 0.0 in
      let sum_x2 = ref 0.0 in
      let sum_y2 = ref 0.0 in
      let pos_ret_sum = ref 0.0 in
      let pos_ret_count = ref 0 in
      let neg_ret_sum = ref 0.0 in
      let neg_ret_count = ref 0 in
      let count = ref 0 in

      for i = 0 to n - lag - 1 do
        let imb = imbalances.(i) in
        let ret = compute_log_return candles.(i).close candles.(i + lag).close in
        sum_xy := !sum_xy +. imb *. ret;
        sum_x := !sum_x +. imb;
        sum_y := !sum_y +. ret;
        sum_x2 := !sum_x2 +. imb *. imb;
        sum_y2 := !sum_y2 +. ret *. ret;
        incr count;
        if imb > 0.0 then begin
          pos_ret_sum := !pos_ret_sum +. ret;
          incr pos_ret_count
        end else if imb < 0.0 then begin
          neg_ret_sum := !neg_ret_sum +. ret;
          incr neg_ret_count
        end
      done;

      let cnt = float !count in
      let mean_x = !sum_x /. cnt in
      let mean_y = !sum_y /. cnt in
      let cov_xy = !sum_xy /. cnt -. mean_x *. mean_y in
      let var_x = !sum_x2 /. cnt -. mean_x *. mean_x in
      let var_y = !sum_y2 /. cnt -. mean_y *. mean_y in
      let corr =
        if var_x > 0.0 && var_y > 0.0 then
          cov_xy /. (sqrt var_x *. sqrt var_y)
        else 0.0
      in

      let pos_mean_ret = if !pos_ret_count > 0 then !pos_ret_sum /. float !pos_ret_count else 0.0 in
      let neg_mean_ret = if !neg_ret_count > 0 then !neg_ret_sum /. float !neg_ret_count else 0.0 in

      Printf.printf "%-8d %-15.6f %-15.8f %-15.8f\n" lag corr pos_mean_ret neg_mean_ret
  ) lags;

  Printf.printf "\n";

  (* Autocorrelation of imbalance *)
  Printf.printf "=== Imbalance Autocorrelation ===\n";
  let ac_lags = [1; 5; 10; 50; 100; 500] in
  List.iter (fun lag ->
    if lag < n then
      let sum_prod = ref 0.0 in
      let sum_sq = ref 0.0 in
      let count = ref 0 in
      for i = 0 to n - lag - 1 do
        sum_prod := !sum_prod +. imbalances.(i) *. imbalances.(i + lag);
        sum_sq := !sum_sq +. imbalances.(i) *. imbalances.(i);
        incr count
      done;
      let ac = if !sum_sq > 0.0 then !sum_prod /. !sum_sq else 0.0 in
      Printf.printf "Lag %-4d: %.6f\n" lag ac
  ) ac_lags
