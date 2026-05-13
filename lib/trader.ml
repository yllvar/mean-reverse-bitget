open Lwt.Infix

type state = {
  prices : float list;
  window_size : int;
  interval : float;
  cycle : int;
}

let init_state ~prices ~window_size ~interval =
  { prices; window_size; interval; cycle = 0 }

let append_price state price =
  let prices = price :: state.prices in
  let prices =
    if List.length prices > state.window_size then
      List.rev (List.tl (List.rev prices))
    else prices
  in
  { state with prices; cycle = state.cycle + 1 }

let format_ts () =
  let t = Unix.time () in
  let tm = Unix.localtime t in
  Printf.sprintf "%02d:%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

let signal_label = function
  | Strategy.Buy -> "BUY  ↓"
  | Strategy.Sell -> "SELL ↑"
  | Strategy.Hold -> "HOLD —"

let rec loop ~stop ~api_key ~secret ~passphrase ~symbol state =
  if !stop then begin
    Printf.printf "\n=== Bot stopped after %d cycles ===\n%!" state.cycle;
    Lwt.return ()
  end else
    Bitget.ticker ~symbol >>= fun ticker_json ->
    match Types.parse_ticker_response ticker_json with
    | _, _, [t] ->
      let current_price = float_of_string t.last_pr in
      let state = append_price state current_price in
      let signal =
        Strategy.mean_reversion ~prices:state.prices ~current_price
      in
      let ts = format_ts () in
      Printf.printf "[%s] #%-4d  Price: %6.2f  Window: %2d  Signal: %s\n%!"
        ts state.cycle current_price (List.length state.prices)
        (signal_label signal);
      Lwt_unix.sleep state.interval >>= fun () ->
      loop ~stop ~api_key ~secret ~passphrase ~symbol state

    | code, msg, _ ->
      Printf.eprintf "Ticker error (code=%s msg=%s), retrying...\n%!" code msg;
      Lwt_unix.sleep state.interval >>= fun () ->
      loop ~stop ~api_key ~secret ~passphrase ~symbol state

let run ~api_key ~secret ~passphrase ~symbol initial_state =
  let stop = ref false in
  let _ = Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ ->
    stop := true;
    print_endline ""
  )) in
  loop ~stop ~api_key ~secret ~passphrase ~symbol initial_state
