open Bitget_lib

let rec take n acc = function
  | _ when n <= 0 -> List.rev acc
  | x :: xs -> take (n - 1) (x :: acc) xs
  | [] -> List.rev acc

let load_env () =
  let chan = open_in ".env" in
  let finally () = close_in chan in
  Fun.protect ~finally (fun () ->
    try
      while true do
        let line = input_line chan in
        match String.split_on_char '=' line with
        | k :: v :: rest ->
          Unix.putenv k (String.concat "=" (v :: rest))
        | _ -> ()
      done
    with End_of_file -> ()
  )

let get_env_nonempty name =
  let v = try Unix.getenv name with Not_found -> "" in
  if v = "" then failwith (Printf.sprintf "%s not set or empty" name)
  else v

let () =
  (try load_env () with _ -> ());
  let api_key = get_env_nonempty "BITGET_API_KEY" in
  let secret = get_env_nonempty "BITGET_SECRET_KEY" in
  let passphrase = get_env_nonempty "BITGET_PASSPHRASE" in

  let window_size = 500 in
  let ema_period = 200 in
  let max_needed = window_size + ema_period in
  print_endline "=== Bitget Mean Reversion Bot ===";
  Printf.eprintf "Fetching %d candles (z-window=%d + EMA=%d)...\n%!" max_needed window_size ema_period;
  let candle_json =
    try Lwt_main.run (Bitget.candles ~symbol:"SOLUSDT" ~granularity:"1min" ~limit:max_needed)
    with exn ->
      failwith (Printf.sprintf "Failed to fetch candles: %s"
        (Printexc.to_string exn))
  in
  let ccode, cmsg, candles =
    try Types.parse_candle_response candle_json
    with exn ->
      failwith (Printf.sprintf "Failed to parse candle response: %s"
        (Printexc.to_string exn))
  in
  if ccode <> "00000" then
    failwith (Printf.sprintf "Candle API error: %s (code=%s)" cmsg ccode);

  let all_prices =
    List.rev (List.filter_map (fun (c : Types.candle) ->
      try Some (float_of_string c.close)
      with Failure _ ->
        Printf.eprintf "Warning: skipping bad candle close: %S\n%!" c.close;
        None
    ) candles)
  in
  if List.length all_prices = 0 then
    failwith "No valid candle close prices received";

  let z_prices = take window_size [] all_prices in
  let ema_prices = take ema_period [] all_prices in

  Printf.printf "Loaded %d prices (%.2f .. %.2f), z-window=%d, ema=%d\n%!"
    (List.length all_prices)
    (List.fold_left min Float.max_float all_prices)
    (List.fold_left max Float.neg_infinity all_prices)
    (List.length z_prices) (List.length ema_prices);

  print_endline "Starting event loop (press Ctrl+C to stop)...\n";

  let state =
    Trader.init_state ~prices:z_prices ~ema_prices ~window_size ~ema_period
      ~threshold:1.0 ~interval:10.0 ~stop_loss:0.50 ~take_profit:1.00
      ~position_size:0.50
  in
  Lwt_main.run (Trader.run ~api_key ~secret ~passphrase ~symbol:"SOLUSDT" state)
