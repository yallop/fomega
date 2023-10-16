module S = Tiny_httpd

let asset_loader path =
  match Content.read path with None -> assert false | Some asset -> asset

let () =
  let server = S.create ~port:8000 () in
  S.add_route_handler ~meth:`GET server S.Route.return (fun _req ->
    S.Response.make_string
      ~headers:[ ("Content-Type", "text/html") ]
      (Ok (asset_loader "index.html")) );

  S.add_route_handler ~meth:`GET server
    S.Route.(exact "jsmain.js" @/ return)
    (fun _req ->
      S.Response.make_string
        ~headers:[ ("Content-Type", "application/javascript") ]
        (Ok (asset_loader "jsmain.js")) );

  S.add_route_handler ~meth:`GET server
    S.Route.(exact "columns.css" @/ return)
    (fun _req ->
      S.Response.make_string
        ~headers:[ ("Content-Type", "text/css") ]
        (Ok (asset_loader "columns.css")) );

  Printf.printf "listening on http://%s:%d\n%!" (S.addr server) (S.port server);
  ignore @@ Sys.command "xdg-open http://localhost:8000";
  ignore (match S.run server with Ok () -> () | Error e -> raise e)
