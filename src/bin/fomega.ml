(* Module Main: The main program.  Deals with processing the command
   line, reading files, building and connecting lexers and parsers, etc.

   For most experiments with the implementation, it should not be
   necessary to change this file.
*)

open Format
open Fomega
open Support.Error
open Syntax

let searchpath = ref [ "" ]

let argDefs =
  [ ( "-I"
    , Arg.String (fun f -> searchpath := f :: !searchpath)
    , "Append a directory to the search path" )
  ]

let parseArgs () =
  let inFiles = ref ([] : string list) in
  Arg.parse argDefs (fun s -> inFiles := s :: !inFiles) "";
  match !inFiles with
  | [] -> err "You must specify input files"
  | files -> files

let openfile infile =
  let rec trynext l =
    match l with
    | [] -> err ("Could not find " ^ infile)
    | d :: rest -> (
      let name = if d = "" then infile else d ^ "/" ^ infile in
      try (name, open_in name) with Sys_error _m -> trynext rest )
  in
  trynext !searchpath

let parseFile inFile (files, ctx) =
  let path, pi = openfile inFile in
  let lexbuf = Lexer.create path pi in
  let result =
    try Parser.toplevel Lexer.main lexbuf
    with Parser.Error -> error (Lexer.info lexbuf) "parse error"
  in
  Parsing.clear_parser ();
  close_in pi;
  let cmds, ctx = result ctx in
  (cmds :: files, ctx)

let main () =
  let inFiles = parseArgs () in
  let files, _ = List.fold_right parseFile inFiles ([], emptycontext) in
  let _ = List.fold_right Process.process_file files emptycontext in
  ()

let () = set_max_boxes 1000

let () = set_margin 67

let res =
  try
    main ();
    0
  with Exit x -> x

let () = print_flush ()

let () = exit res
