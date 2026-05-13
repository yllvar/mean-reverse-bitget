open Lwt.Infix

let base_url = "https://api.bitget.com"

let balance ~api_key ~secret ~passphrase =
  let path = "/api/v2/spot/account/assets" in
  let url = base_url ^ path in
  let headers = Auth.headers ~api_key ~secret ~passphrase ~method_:"GET" ~path ~body:"" in
  Client.get ~headers url

let ticker ~symbol =
  let path = Printf.sprintf "/api/v2/spot/market/tickers?symbol=%s" symbol in
  Client.get ~headers:[] (base_url ^ path)

let orderbook ~symbol ~limit =
  let path = Printf.sprintf "/api/v2/spot/market/orderbook?symbol=%s&limit=%d" symbol limit in
  Client.get ~headers:[] (base_url ^ path)

let candles ~symbol ~granularity ~limit =
  let path =
    Printf.sprintf "/api/v2/spot/market/candles?symbol=%s&granularity=%s&limit=%d"
      symbol granularity limit
  in
  Client.get ~headers:[] (base_url ^ path)

let place_limit_order ~api_key ~secret ~passphrase ~symbol ~side ~price ~quantity =
  let path = "/api/v2/spot/trade/place-order" in
  let client_oid =
    Printf.sprintf "bt_%s_%d" side (int_of_float (Unix.gettimeofday () *. 1000.0))
  in
  let body =
    Yojson.Safe.to_string
      (`Assoc
        [
          ("symbol", `String symbol);
          ("side", `String side);
          ("orderType", `String "limit");
          ("force", `String "post_only");
          ("price", `String price);
          ("quantity", `String quantity);
          ("clientOrderId", `String client_oid);
        ])
  in
  let headers =
    Auth.headers ~api_key ~secret ~passphrase ~method_:"POST" ~path ~body
  in
  Client.post ~headers ~body (base_url ^ path)
