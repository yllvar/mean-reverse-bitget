type signal = Buy | Sell | Hold

let apply_trend_filter signal price trend =
  if abs_float trend < 0.01 then signal
  else match signal with
  | Buy when price <= trend -> Hold
  | Sell when price >= trend -> Hold
  | _ -> signal

let ema ~prices ~period =
  match List.rev prices with
  | [] -> 0.0
  | first :: rest ->
    let k = 2.0 /. (float period +. 1.0) in
    let _, ema_v =
      List.fold_left (fun (prev, _) p ->
        let e = p *. k +. prev *. (1.0 -. k) in
        (e, e)
      ) (first, first) rest
    in
    ema_v

let mean_reversion ~prices ~current_price ~threshold =
  let n = List.length prices in
  if n < 5 then (Hold, 0.)
  else
    let sum = List.fold_left (+.) 0. prices in
    let mean = sum /. float n in
    let variance =
      List.fold_left (fun acc p -> acc +. (p -. mean) ** 2.) 0. prices /. float n
    in
    let stddev = sqrt variance in
    if stddev < 0.01 then (Hold, 0.)
    else
      let z = (current_price -. mean) /. stddev in
      if z < -.threshold then (Buy, z)
      else if z > threshold then (Sell, z)
      else (Hold, z)
