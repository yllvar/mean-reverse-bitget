open Lwt.Infix

let get ~headers uri_str =
  let headers = Cohttp.Header.of_list headers in
  let uri = Uri.of_string uri_str in
  Cohttp_lwt_unix.Client.get ~headers uri
  >>= fun (_, body) ->
  Cohttp_lwt.Body.to_string body

let post ~headers ~body uri_str =
  let headers = Cohttp.Header.of_list headers in
  let uri = Uri.of_string uri_str in
  let body_cohttp = Cohttp_lwt.Body.of_string body in
  Cohttp_lwt_unix.Client.post ~headers ~body:body_cohttp uri
  >>= fun (_, body) ->
  Cohttp_lwt.Body.to_string body
