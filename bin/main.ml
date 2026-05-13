open Bitget_lib

let load_env () =
  let chan = open_in ".env" in
  let finally () = close_in chan in
  Fun.protect ~finally (fun () ->
    try
      while true do
        let line = input_line chan in
        match String.split_on_char '=' line with
        | [k; v] -> Unix.putenv k v
        | _ -> ()
      done
    with End_of_file -> ()
  )

let () =
  (try load_env () with _ -> ());
  let api_key =
    try Unix.getenv "BITGET_API_KEY"
    with Not_found -> failwith "BITGET_API_KEY not set"
  in
  let secret =
    try Unix.getenv "BITGET_SECRET_KEY"
    with Not_found -> failwith "BITGET_SECRET_KEY not set"
  in
  let passphrase =
    try Unix.getenv "BITGET_PASSPHRASE"
    with Not_found -> failwith "BITGET_PASSPHRASE not set"
  in

  (* Initialize price history from candles *)
  print_endline "=== Bitget Mean Reversion Bot ===";
  print_endline "Fetching initial price window (20 candles)...";
  let candle_json =
    Lwt_main.run (Bitget.candles ~symbol:"SOLUSDT" ~granularity:"1min" ~limit:20)
  in
  let ccode, cmsg, candles = Types.parse_candle_response candle_json in
  if ccode <> "00000" then failwith (Printf.sprintf "Candle API error: %s" cmsg);

  let close_prices =
    List.rev_map (fun (c : Types.candle) -> float_of_string c.close) candles
  in
  Printf.printf "Loaded %d prices (%.2f .. %.2f)\n%!"
    (List.length close_prices)
    (List.fold_left min Float.max_float close_prices)
    (List.fold_left max Float.neg_infinity close_prices);

  print_endline "Starting event loop (press Ctrl+C to stop)...\n";

  let state =
    Trader.init_state ~prices:close_prices ~window_size:20 ~interval:10.0
  in
  Lwt_main.run (Trader.run ~api_key ~secret ~passphrase ~symbol:"SOLUSDT" state)
