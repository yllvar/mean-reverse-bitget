type signal = Buy | Sell | Hold

let mean_reversion ~prices ~current_price =
  let n = List.length prices in
  if n < 5 then Hold
  else
    let sum = List.fold_left (+.) 0. prices in
    let mean = sum /. float n in
    let variance =
      List.fold_left (fun acc p -> acc +. (p -. mean) ** 2.) 0. prices /. float n
    in
    let stddev = sqrt variance in
    if stddev < 0.01 then Hold
    else
      let z = (current_price -. mean) /. stddev in
      if z < -1.5 then Buy
      else if z > 1.5 then Sell
      else Hold
