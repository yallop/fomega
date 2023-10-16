open Format
open Support.Pervasive
open Syntax
open Core

let process_command ctx cmd = match cmd with
  | Eval(_fi,t) ->
      let tyT = typeof ctx t in
      let t' = eval ctx t in
      prresult ctx (prettify t') (prettifyty tyT);
      force_newline();
      ctx
  | Bind(fi,x,bind) ->
      let bind = checkbinding fi ctx bind in
      let bind = evalbinding ctx bind in
      pr x; pr " ";
      prbinding ctx (prettifybinding bind);
      force_newline();
      addbinding ctx x bind
  | Open(fi, t, tyX, x) ->
      let knd, ty = typeopen fi ctx t in
      let tybind = TyVarBind knd in
      let varbind =
        match evalopen ctx t with
        | Some t -> VarDef(t, Some ty)
        | None -> VarBind ty
      in
      pr tyX; pr " ";
      prbinding ctx (prettifybinding tybind);
      force_newline();
      let ctx = addbinding ctx tyX tybind in
      pr x; pr " ";
      prbinding ctx (prettifybinding varbind);
      force_newline();
      addbinding ctx x varbind

let process_file cmds ctx =
  let g ctx c =
    open_hvbox 0;
    let ctx = process_command ctx c in
    print_flush();
    ctx
  in
    List.fold_left g ctx cmds
