open Format

module Error = struct

exception Exit of int

type info = FI of string * int * int * int * int | UNKNOWN
type 'a withinfo = {i: info; v: 'a}

let dummyinfo = UNKNOWN
let createInfo f ls cs le ce = FI(f, ls, cs, le, ce)

let merge i1 i2 = match i1, i2 with
  | FI (f, ls, cs, _, _), FI (_, _, _, le, ce) -> FI (f, ls, cs, le, ce)
  | UNKNOWN, _ | _, UNKNOWN -> UNKNOWN

let errf f =
  print_flush();
  print_newline();
  open_vbox 0;
  open_hvbox 0;
  f();
  print_cut();
  close_box();
  print_newline();
  raise (Exit 1)

let printInfo =
  (* In the text of the book, file positions in error messages are replaced
     with the string "Error:" *)
  function
    FI(f,ls,cs,le,ce) ->
      if (String.length f <> 0) then begin
        print_string f;
        print_string ":";
      end;
      print_int ls; print_string ".";
      print_int cs; print_string "-";
      if le <> ls then
        (print_int le;prerr_string ".");
      print_int ce; print_string ":"
  | UNKNOWN ->
      print_string "<Unknown file and line>: "

let errfAt fi f = errf(fun()-> printInfo fi; print_space(); f())

let err s = errf (fun()-> print_string "Error: "; print_string s)

let error fi s = errfAt fi (fun()-> print_string s)

let warning s =
  print_string "Warning: "; print_string s;
  print_newline()

let warningAt fi s =
  printInfo fi; print_string " Warning: ";
  print_string s; print_newline()

end

(* ---------------------------------------------------------------------- *)

module Pervasive = struct

type info = Error.info

let pr = Format.print_string

end (* module pervasive *)
