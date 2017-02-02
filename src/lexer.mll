(* The lexical analyzer. *)

{
open Support.Error

let lineno   = ref 1
and start    = ref 0

and filename = ref ""
and startLex = ref dummyinfo

(* To handle glyphs *)
let curr_offset = ref 0

let create inFile stream =
  if not (Filename.is_implicit inFile) then filename := inFile
  else filename := Filename.concat (Sys.getcwd()) inFile;
  lineno := 1; start := 0; Lexing.from_channel stream

let from_string s =
  filename := ""; lineno := 1; start := 0;
  Lexing.from_string s

let newline lexbuf = incr lineno; curr_offset := 0; start := (Lexing.lexeme_start lexbuf)

let info ?(offset = 0) lexbuf =
  let r = !curr_offset - !start in
  curr_offset := !curr_offset - offset;
  createInfo (!filename)
    (!lineno) (Lexing.lexeme_start lexbuf + r)
    (!lineno) (Lexing.lexeme_end lexbuf + r - offset - 1)

let text = Lexing.lexeme

let int_of_subdigit_string str =
  let len = String.length str in
  let rec loop i =
    if i < len then begin
      assert (str.[i] = '\226');
      assert (str.[i+1] = '\130');
      let digit =
        match str.[i+2] with
          | '\128' -> 0
          | '\129' -> 1
          | '\130' -> 2
          | '\131' -> 3
          | '\132' -> 4
          | '\133' -> 5
          | '\134' -> 6
          | '\135' -> 7
          | '\136' -> 8
          | '\137' -> 9
          | _ -> assert false
      in
        digit :: (loop (i+3))
    end else []
  in
  let digits = loop 0 in
  let _, res =
    List.fold_right
      (fun d (base, acc) ->
         (base * 10, base * d + acc))
      digits (1, 0)
  in
  res
}

let whitespace = [' ' '	']
let idchar =  ['A'-'Z' 'a'-'z' '_' '0'-'9' '\'']
let lident = ['a'-'z' '_'] idchar*
let uident = ['A'-'Z'] idchar*
let lcgreek = 
    "\206\177" (* α *)
  | "\206\178" (* β *)
  | "\206\179" (* γ *)
  | "\206\180" (* δ *)
  | "\206\181" (* ε *)
  | "\206\182" (* ζ *)
  | "\206\183" (* η *)
  | "\206\184" (* θ *)
  | "\206\185" (* ι *)
  | "\206\186" (* κ *)

  | "\206\188" (* μ *)
  | "\206\189" (* ν *)
  | "\206\190" (* ξ *)
  | "\206\191" (* ο *)
  | "\207\128" (* π *)
  | "\207\129" (* ρ *)
  | "\207\130" (* ς *)
  | "\207\131" (* σ *)
  | "\207\132" (* τ *)
  | "\207\133" (* υ *)
  | "\207\134" (* φ *)
  | "\207\135" (* χ *)
  | "\207\136" (* ψ *)
  | "\207\137" (* ω *)

let comment = ['#'] [^'\n'] *

let digit = ['0'-'9']
let firstdigit = ['1'-'9']
let index = firstdigit digit*

let subdigit = "\226\130"['\128'-'\137']
let firstsubdigit = "\226\130"['\129'-'\137']
let subindex = firstsubdigit subdigit*

(* The main body of the lexical analyzer *)

rule main = parse
  whitespace+                       { main lexbuf }
| whitespace*("\r")?"\n"            { newline lexbuf; main lexbuf }
| comment                           { main lexbuf }
| "->"                              { Parser.ARROW (info lexbuf) }
| "\226\134\146" (* → *)           { Parser.ARROW (info ~offset:2 lexbuf) }
| "=>"                              { Parser.DARROW (info lexbuf) }
| "\226\135\146" (* ⇒ *)           { Parser.DARROW (info ~offset:2 lexbuf) }
| "lambda"                          { Parser.LAMBDA (info lexbuf) }
| "\206\187" (* λ *)                { Parser.LAMBDA (info ~offset:1 lexbuf) }
| "LAMBDA"                          { Parser.BIGLAMBDA (info lexbuf) }
| "\206\155" (* Λ *)                { Parser.BIGLAMBDA (info ~offset:1 lexbuf) }
| "All"                             { Parser.ALL (info lexbuf) }
| "\226\136\128" (* ∀ *)           { Parser.ALL (info ~offset:2 lexbuf) }
| "EXISTS"                          { Parser.EXISTS (info lexbuf) }
| "\226\136\131" (* ∃ *)           { Parser.EXISTS (info ~offset:2 lexbuf) }
| "pack"                            { Parser.PACK (info lexbuf) }
| "open"                            { Parser.OPEN (info lexbuf) }
| "case"                            { Parser.CASE (info lexbuf) }
| "inl"                             { Parser.INL (info lexbuf) }
| "inr"                             { Parser.INR (info lexbuf) }
| "as"                              { Parser.AS (info lexbuf) }
| "in"                              { Parser.IN (info lexbuf) }
| "of"                              { Parser.OF (info lexbuf) }
| "_"                               { Parser.USCORE (info lexbuf) }
| "="                               { Parser.EQ (info lexbuf) }
| ","                               { Parser.COMMA (info lexbuf) }
| "*"                               { Parser.STAR (info lexbuf) }
| "."                               { Parser.DOT (info lexbuf) }
| ";"                               { Parser.SEMI (info lexbuf) }
| ":"                               { Parser.COLON (info lexbuf) }
| "::"                              { Parser.COLONCOLON (info lexbuf) }
| "["                               { Parser.LSQUARE (info lexbuf) }
| "("                               { Parser.LPAREN (info lexbuf) }
| ")"                               { Parser.RPAREN (info lexbuf) }
| "]"                               { Parser.RSQUARE (info lexbuf) }
| "<"                               { Parser.LANGLE (info lexbuf) }
| "\226\159\168" (* ⟨ *)
| "\226\140\169" (* 〈 *)            { Parser.LANGLE (info ~offset:2 lexbuf) }
| ">"                               { Parser.RANGLE (info lexbuf) }
| "\226\159\169" (* ⟩ *)
| "\226\140\170" (* 〉 *)            { Parser.RANGLE (info ~offset:2 lexbuf) }
| "&"                               { Parser.TIMES (info lexbuf) }
| "\195\151" (* × *)                { Parser.TIMES (info ~offset:1 lexbuf) }
| "+"                               { Parser.PLUS (info lexbuf) }
| "|"                               { Parser.VBAR (info lexbuf) }
| "@"                               { Parser.AT (info lexbuf) }
| "\207\128" (* π *)                { Parser.PI (info ~offset:1 lexbuf) }
| lident as id                      { Parser.LCID {i=info lexbuf;v=id} }
| uident as id                      { Parser.UCID {i=info lexbuf;v=id} }
| lcgreek as id                     { Parser.UCID {i=info ~offset:(String.length id / 2) lexbuf;v=id} }
| index as idx                      { Parser.INDEX
                                        {i=info lexbuf;
                                         v=int_of_string idx} }
| subindex as idx                   { Parser.SUBINDEX
                                        {i=info ~offset:(String.length idx / 3) lexbuf;
                                         v=int_of_subdigit_string idx} }
| eof                               { Parser.EOF(info lexbuf) }
| _ as c                            { error (info lexbuf)
                                        (Printf.sprintf "Illegal character %c" c) }

(*  *)
