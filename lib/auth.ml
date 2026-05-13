let timestamp_ms () =
  let open Unix in
  let tv = gettimeofday () in
  string_of_int (int_of_float (tv *. 1000.0))

let sign ~secret ~timestamp ~method_ ~path ~body =
  let message = timestamp ^ String.uppercase_ascii method_ ^ path ^ body in
  let open Digestif.SHA256 in
  hmac_string ~key:secret message |> to_raw_string |> Base64.encode_string

let headers ~api_key ~secret ~passphrase ~method_ ~path ~body =
  let ts = timestamp_ms () in
  let signature = sign ~secret ~timestamp:ts ~method_ ~path ~body in
  [
    ("ACCESS-KEY", api_key);
    ("ACCESS-SIGN", signature);
    ("ACCESS-TIMESTAMP", ts);
    ("ACCESS-PASSPHRASE", passphrase);
  ]
