type balance_item = {
  coin : string;
  available : string;
  frozen : string;
}

type balance_response = {
  code : string;
  msg : string;
  data : balance_item list;
}

type ticker_item = {
  symbol : string;
  last_pr : string;
  high_24h : string;
  low_24h : string;
  base_volume : string;
  usdt_volume : string;
}

type orderbook_level = { price : string; amount : string }

type candle = {
  ts : string;
  open_p : string;
  high : string;
  low : string;
  close : string;
  volume : string;
}

type order_result = {
  order_id : string;
  client_order_id : string;
}

let parse_balance_response json_str =
  let json = Yojson.Safe.from_string json_str in
  let open Yojson.Safe.Util in
  let code = json |> member "code" |> to_string in
  let msg = json |> member "msg" |> to_string in
  let data =
    json |> member "data" |> to_list |> List.map (fun item ->
      {
        coin = item |> member "coinName" |> to_string;
        available = item |> member "available" |> to_string;
        frozen = item |> member "frozen" |> to_string;
      })
  in
  { code; msg; data }

let parse_ticker_response json_str =
  let json = Yojson.Safe.from_string json_str in
  let open Yojson.Safe.Util in
  let code = json |> member "code" |> to_string in
  let msg = json |> member "msg" |> to_string in
  let data =
    json |> member "data" |> to_list |> List.map (fun item ->
      {
        symbol = item |> member "symbol" |> to_string;
        last_pr = item |> member "lastPr" |> to_string;
        high_24h = item |> member "high24h" |> to_string;
        low_24h = item |> member "low24h" |> to_string;
        base_volume = item |> member "baseVolume" |> to_string;
        usdt_volume = item |> member "usdtVolume" |> to_string;
      })
  in
  (code, msg, data)

let parse_orderbook_response json_str =
  let json = Yojson.Safe.from_string json_str in
  let open Yojson.Safe.Util in
  let code = json |> member "code" |> to_string in
  let msg = json |> member "msg" |> to_string in
  let parse_level level =
    let l = level |> to_list in
    {
      price = List.nth l 0 |> to_string;
      amount = List.nth l 1 |> to_string;
    }
  in
  let data = json |> member "data" in
  let asks = data |> member "asks" |> to_list |> List.map parse_level in
  let bids = data |> member "bids" |> to_list |> List.map parse_level in
  (code, msg, asks, bids)

let parse_candle_response json_str =
  let json = Yojson.Safe.from_string json_str in
  let open Yojson.Safe.Util in
  let code = json |> member "code" |> to_string in
  let msg = json |> member "msg" |> to_string in
  let data =
    json |> member "data" |> to_list |> List.map (fun item ->
      let fields = item |> to_list in
      {
        ts = List.nth fields 0 |> to_string;
        open_p = List.nth fields 1 |> to_string;
        high = List.nth fields 2 |> to_string;
        low = List.nth fields 3 |> to_string;
        close = List.nth fields 4 |> to_string;
        volume = List.nth fields 5 |> to_string;
      })
  in
  (code, msg, data)

let parse_order_response json_str =
  let json = Yojson.Safe.from_string json_str in
  let open Yojson.Safe.Util in
  let code = json |> member "code" |> to_string in
  let msg = json |> member "msg" |> to_string in
  let data = json |> member "data" in
  let order_id = data |> member "orderId" |> to_string in
  let client_order_id = data |> member "clientOrderId" |> to_string in
  (code, msg, { order_id; client_order_id })
