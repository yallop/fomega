open Format
open Support.Error
open Support.Pervasive

(* ---------------------------------------------------------------------- *)
(* Datatypes *)

type kind =
    KnStar
  | KnArr of kind * kind

type ty =
    TyVar of int * int
  | TyArr of ty * ty
  | TyAll of string * kind * ty
  | TyExi of string * kind * ty
  | TyAbs of string * kind * ty
  | TyApp of ty * ty
  | TyProd of ty list
  | TySum of ty * ty

type inj = L | R

type term =
    TmVar of info * int * int
  | TmAbs of info * string * ty * term
  | TmApp of info * term * term
  | TmPac of info * ty * term * string * kind * ty
  | TmOpe of info * term * string * string * term
  | TmTAbs of info * string * kind * term
  | TmTApp of info * term * ty
  | TmProd of info * term list
  | TmProj of info * term * int
  | TmCase of info * term * string * term * string * term
  | TmInj of info * ty * inj * term

type binding =
    NameBind
  | TyVarBind of kind
  | VarBind of ty
  | TyVarDef of ty * kind option
  | VarDef of term * ty option

type context = (string * binding) list

type command =
  | Eval of info * term
  | Bind of info * string * binding
  | Open of info * term * string * string

(* ---------------------------------------------------------------------- *)
(* Context management *)

let emptycontext = []

let ctxlength ctx = List.length ctx

let nthbinding ctx i = List.nth ctx i

let addbinding ctx x bind = (x,bind) :: ctx

let addname ctx x = addbinding ctx x NameBind

let rec isnamebound ctx x =
  match ctx with
  | [] -> false
  | (y,_)::rest ->
      if y=x then true
      else isnamebound rest x

let rec pickfreshname ctx x =
  if isnamebound ctx x then pickfreshname ctx (x^"'")
  else ((x,NameBind) :: ctx, x)

let index2name fi ctx x =
  try
    let (xn,_) = List.nth ctx x in
    xn
  with Failure _ ->
    let msg =
      Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d" in
    error fi (msg x (List.length ctx))

let rec name2index fi ctx x =
  match ctx with
  | [] -> error fi ("Identifier " ^ x ^ " is unbound")
  | (y,_)::rest ->
      if y=x then 0
      else 1 + (name2index fi rest x)

(* ---------------------------------------------------------------------- *)
(* Shifting *)

let tymap onvar c tyT =
  let rec walk c tyT = match tyT with
    TyVar(x,n) -> onvar c x n
  | TyArr(tyT1,tyT2) -> TyArr(walk c tyT1,walk c tyT2)
  | TyAll(tyX,knK1,tyT2) -> TyAll(tyX,knK1,walk (c+1) tyT2)
  | TyExi(tyX,knK1,tyT2) -> TyExi(tyX,knK1,walk (c+1) tyT2)
  | TyAbs(tyX,knK1,tyT2) -> TyAbs(tyX,knK1,walk (c+1) tyT2)
  | TyApp(tyT1,tyT2) -> TyApp(walk c tyT1,walk c tyT2)
  | TyProd tys -> TyProd (List.map (walk c) tys)
  | TySum (ty1,ty2) -> TySum(walk c ty1, walk c ty2)
  in walk c tyT

let tmmap onvar ontype c t =
  let rec walk c t = match t with
    TmVar(fi,x,n) -> onvar fi c x n
  | TmAbs(fi,x,tyT1,t2) -> TmAbs(fi,x,ontype c tyT1,walk (c+1) t2)
  | TmApp(fi,t1,t2) -> TmApp(fi,walk c t1,walk c t2)
  | TmPac(fi,ty1,tm,x,k,ty2) ->
      TmPac(fi,ontype c ty1,walk c tm,x,k,ontype (c+1) ty2)
  | TmOpe(fi,t1,x,y,t2) -> TmOpe(fi,walk c t1,x,y,walk (c+2) t2)
  | TmTAbs(fi,tyX,knK1,t2) -> TmTAbs(fi,tyX,knK1,walk (c+1) t2)
  | TmTApp(fi,t1,tyT2) -> TmTApp(fi,walk c t1,ontype c tyT2)
  | TmProd(fi,tms) -> TmProd (fi, List.map (walk c) tms)
  | TmProj(fi,tm,i) -> TmProj (fi, walk c tm, i)
  | TmCase(fi,tm1,x,tm2,y,tm3) ->
    TmCase(fi,walk c tm1,x,walk (c+1) tm2,y,walk (c+1) tm3)
  | TmInj(fi,ty,i,tm) -> TmInj (fi,ontype c ty,i,walk c tm)
  in walk c t

let typeShiftAbove d c tyT =
  tymap
    (fun c x n ->
       if x>=c then
         if x+d < 0 then err "scoping error"
         else TyVar(x+d,n+d)
       else TyVar(x,n+d))
    c tyT

let termShiftAbove d c t =
  tmmap
    (fun fi c x n -> if x>=c then TmVar(fi,x+d,n+d)
                     else TmVar(fi,x,n+d))
    (typeShiftAbove d)
    c t

let termShift d t = termShiftAbove d 0 t

let typeShift d tyT = typeShiftAbove d 0 tyT

let bindingshift d bind =
  match bind with
    NameBind -> NameBind
  | TyVarBind(knK) -> TyVarBind(knK)
  | TyVarDef(tyT, opt) -> TyVarDef(typeShift d tyT, opt)
  | VarBind(tyT) -> VarBind(typeShift d tyT)
  | VarDef(t, tyT_opt) ->
      let t' = termShift d t in
      let tyT_opt' =
        match tyT_opt with
        | None -> None
        | Some(tyT) -> Some(typeShift d tyT)
      in
      VarDef(t', tyT_opt')

(* ---------------------------------------------------------------------- *)
(* Substitution *)

let termSubst j s t =
  tmmap
    (fun fi j x n -> if x=j then termShift j s else TmVar(fi,x,n))
    (fun _j tyT -> tyT)
    j t

let termSubstTop s t =
  termShift (-1) (termSubst 0 (termShift 1 s) t)

let typeSubst tyS j tyT =
  tymap
    (fun j x n -> if x=j then (typeShift j tyS) else (TyVar(x,n)))
    j tyT

let typeSubstTop tyS tyT =
  typeShift (-1) (typeSubst (typeShift 1 tyS) 0 tyT)

let tytermSubst tyS j t =
  tmmap (fun fi _c x n -> TmVar(fi,x,n))
        (fun j tyT -> typeSubst tyS j tyT) j t

let tytermSubstTop tyS t =
  termShift (-1) (tytermSubst (typeShift 1 tyS) 0 t)

(* ---------------------------------------------------------------------- *)
(* Context management (continued) *)

let getbinding fi ctx i =
  try
    let (_,bind) = nthbinding ctx i in
    bindingshift (i+1) bind
  with Failure _ ->
    let msg =
      Printf.sprintf "variable lookup failure: offset: %d, ctx size: %d" in
    error fi (msg i (ctxlength ctx))

let getTypeFromContext fi ctx i =
   match getbinding fi ctx i with
   | VarBind tyT -> tyT
   | VarDef(_, Some tyT) -> tyT
   | VarDef(_,None) ->
       error fi ("no type recorded for variable " ^ (index2name fi ctx i))
   | _ ->
       error fi ("getTypeFromContext: Wrong kind of binding for variable "
                 ^ (index2name fi ctx i))

let getKindFromContext fi ctx i =
  match getbinding fi ctx i with
  | TyVarBind knK -> knK
  | TyVarDef(_, Some knK) -> knK
  | TyVarDef(_, None) ->
      error fi ("no kind recorded for variable " ^ (index2name fi ctx i))
  | _ ->
      error fi ("getKindFromContext: Wrong kind of binding for variable "
                ^ (index2name fi ctx i))

(* ---------------------------------------------------------------------- *)
(* Extracting file info *)

let tmInfo t = match t with
    TmVar(fi,_,_) -> fi
  | TmAbs(fi,_,_,_) -> fi
  | TmApp(fi, _, _) -> fi
  | TmPac(fi,_,_,_,_,_) -> fi
  | TmOpe(fi,_,_,_,_) -> fi
  | TmTAbs(fi,_,_,_) -> fi
  | TmTApp(fi,_, _) -> fi
  | TmProd(fi,_) -> fi
  | TmProj(fi,_,_) -> fi
  | TmCase(fi,_,_,_,_,_) -> fi
  | TmInj(fi,_,_,_) -> fi

(* ---------------------------------------------------------------------- *)
(* Printing *)

(* The printing functions call these utility functions to insert grouping
  information and line-breaking hints for the pretty-printing library:
     obox   Open a "box" whose contents will be indented by two spaces if
            the whole box cannot fit on the current line
     obox0  Same but indent continuation lines to the same column as the
            beginning of the box rather than 2 more columns to the right
     cbox   Close the current box
     break  Insert a breakpoint indicating where the line maybe broken if
            necessary.
  See the documentation for the Format module in the OCaml library for
  more details.
*)

let lambda_glyph = "\206\187"
and biglambda_glyph = "\206\155"
and forall_glyph = "\226\136\128"
and exists_glyph = "\226\136\131"
and arrow_glyph = "\226\134\146"
and darrow_glyph = "\226\135\146"
and times_glyph = "\195\151"
and langle_glyph = "\226\140\169"
and rangle_glyph = "\226\140\170"
and pi_glyph = "\207\128"

let index_glyph idx =
  let rec loop i =
    if i = 0 then []
    else (loop (i / 10)) @ [i mod 10]
  in
  let digits = loop idx in
  let digit_glyph = function
    | 0 -> "\226\130\128"
    | 1 -> "\226\130\129"
    | 2 -> "\226\130\130"
    | 3 -> "\226\130\131"
    | 4 -> "\226\130\132"
    | 5 -> "\226\130\133"
    | 6 -> "\226\130\134"
    | 7 -> "\226\130\135"
    | 8 -> "\226\130\136"
    | 9 -> "\226\130\137"
    | _ -> assert false
  in
  let buf = Buffer.create 3 in
  List.iter
    (fun i -> Buffer.add_string buf (digit_glyph i))
    digits;
  Buffer.contents buf

let obox0() = open_hvbox 0
let obox() = open_hvbox 2
let cbox() = close_box()
let break() = print_break 0 0

let small t =
  match t with
    TmVar(_,_,_) -> true
  | _ -> false

let rec printkn_kind outer ctx k = match k with
      knK -> printkn_arrowkind outer ctx knK

and printkn_arrowkind outer ctx k = match k with
    KnArr(knK1,knK2) ->
      obox0();
      printkn_akind false ctx knK1;
      if outer then pr " ";
      pr darrow_glyph;
      if outer then print_space() else break();
      printkn_arrowkind outer ctx knK2;
      cbox()
  | knK -> printkn_akind outer ctx knK

and printkn_akind outer ctx k = match k with
    KnStar -> pr "*"
  | knK -> pr "("; printkn_kind outer ctx knK; pr ")"

let printkn ctx knK = printkn_kind true ctx knK

let prokn ctx knK =
  if knK <> KnStar then (pr "::"; printkn_kind false ctx knK)

let rec printty_Type outer ctx tyT = match tyT with
    TyAll(tyX,knK1,tyT2) ->
      let (ctx1,tyX) = (pickfreshname ctx tyX) in
      obox(); pr forall_glyph; pr tyX; prokn ctx knK1; pr ".";
      printty_Type outer ctx1 tyT2;
      cbox()
  | TyExi(tyX,knK1,tyT2) ->
      let (ctx1,tyX) = (pickfreshname ctx tyX) in
      obox(); pr exists_glyph; pr tyX; prokn ctx knK1; pr ".";
      printty_Type false ctx1 tyT2;
      cbox()
  | TyAbs(tyX,knK1,tyT2) ->
      let (ctx',x') = (pickfreshname ctx tyX) in
      obox(); pr lambda_glyph;
      pr x'; prokn ctx knK1;
      pr "."; if not outer then break();
      printty_Type outer ctx' tyT2;
      cbox()
  | tyT -> printty_ArrowType outer ctx tyT

and printty_ArrowType outer ctx tyT = match tyT with
    TyArr(tyT1,tyT2) ->
      obox0();
      printty_SumType false ctx tyT1;
      if outer then pr " ";
      pr arrow_glyph;
      if outer then print_space() else break();
      printty_ArrowType outer ctx tyT2;
      cbox()
  | tyT -> printty_SumType outer ctx tyT

and printty_SumType outer ctx tyT = match tyT with
  | TySum (tyL, tyR) ->
      obox0();
      printty_ProdType false ctx tyL;
      if outer then pr " ";
      pr "+";
      if outer then print_space() else break();
      printty_SumType outer ctx tyR;
      cbox()
  | tyT -> printty_ProdType outer ctx tyT

and printty_ProdType outer ctx tyT = match tyT with
  | TyProd tys ->
      let rec loop l = match l with
          [] -> ()
        | [ty] -> printty_AppType false ctx ty
        | ty::rest ->
            printty_AppType false ctx ty;
            if outer then pr " ";
            pr times_glyph;
            if outer then print_space() else break();
            loop rest
      in
      obox0 ();
      loop tys;
      cbox()
  | tyT -> printty_AppType outer ctx tyT

and printty_AppType outer ctx k = match k with
    TyApp(tyT1,tyT2) ->
      obox0();
      printty_AppType false ctx tyT1;
      print_space();
      printty_AType false ctx tyT2;
      cbox()
  | tyT -> printty_AType outer ctx tyT

and printty_AType outer ctx tyT = match tyT with
    TyVar(x,n) ->
      if ctxlength ctx = n then
        pr (index2name dummyinfo ctx x)
      else
        pr ("[bad index: " ^ (string_of_int x) ^ "/" ^ (string_of_int n)
            ^ " in {"
            ^ (List.fold_left (fun s (x,_) -> s ^ " " ^ x) "" ctx)
            ^ " }]")
  | tyT -> pr "("; printty_Type outer ctx tyT; pr ")"

let printty ctx tyT = printty_Type true ctx tyT

let rec printtm_Term outer ctx t = match t with
    TmAbs(_fi,x,tyT1,t2) ->
      (let (ctx',x') = (pickfreshname ctx x) in
         obox(); pr lambda_glyph;
         pr x'; pr ":"; printty_Type false ctx tyT1; pr ".";
         if (small t2) && not outer then break();
         printtm_Term outer ctx' t2;
         cbox())
  | TmCase(_fi,t,x,t1,y,t2) ->
      obox0();
      pr "case ";
      printtm_Term false ctx t;
      pr " of "; pr x; printtm_Term false ctx t1;
      pr " | ";  pr y; printtm_Term false ctx t2;
      cbox();
  | TmTAbs(_fi,x,knK,t) ->
      (let (ctx1,x) = (pickfreshname ctx x) in
            obox(); pr biglambda_glyph; pr x;
            prokn ctx knK;
            pr ".";
            if (small t) && not outer then break();
            printtm_Term outer ctx1 t;
            cbox())
  | TmPac(_fi,ty1,tm,x,k,ty2) ->
      obox (); pr "pack ";
      obox ();
      printty_Type false ctx ty1; pr ",";
      break();
      printtm_Term outer ctx tm;
      cbox ();
      pr " as ";
      printty_Type false ctx (TyExi (x,k,ty2));
      cbox ()
  | TmOpe(_fi,t1,x,y,t2) ->
    (let (ctx1,x) = (pickfreshname ctx x) in
     let (ctx2,y) = (pickfreshname ctx1 y) in
      obox (); pr "open ";
      printtm_Term outer ctx t1; pr " as ";
      pr x; pr ", "; pr y;
      print_space();
      pr "in";
      print_space();
      printtm_Term outer ctx2 t2;
      cbox ())
  | t -> printtm_AppTerm outer ctx t

and printtm_AppTerm outer ctx t = match t with
    TmApp(_fi, t1, t2) ->
      obox0();
      printtm_AppTerm false ctx t1;
      print_space();
      printtm_ATerm false ctx t2;
      cbox()
  | TmTApp(_fi,t,tyS) ->
      obox0();
      printtm_AppTerm false ctx t;
      print_space();
      pr "["; printty_Type false ctx tyS; pr "]";
      cbox()
  | TmProj(_fi, t1, i) ->
      obox0();
      pr pi_glyph;
      pr (index_glyph i);
      print_space();
      printtm_ATerm false ctx t1;
      cbox()
  | TmInj(_fi, ty, i, tm) ->
      obox0();
      pr (match i with L -> "inl" | R -> "inr");
      print_space();
      pr "["; printty_Type false ctx ty; pr "]";
      print_space();
      printtm_ATerm false ctx tm;
      cbox();
  | t -> printtm_ATerm outer ctx t

and printtm_ATerm outer ctx t = match t with
    TmVar(fi,x,n) ->
      if ctxlength ctx = n then
        pr (index2name fi ctx x)
      else
        pr ("[bad index: " ^ (string_of_int x) ^ "/" ^ (string_of_int n)
            ^ " in {"
            ^ (List.fold_left (fun s (x,_) -> s ^ " " ^ x) "" ctx)
            ^ " }]")
  | TmProd(_fi, tms) ->
      let rec loop l = match l with
          [] -> ()
        | [tm] -> printtm_Term false ctx tm
        | tm::rest ->
            printtm_Term false ctx tm;
            pr",";
            if outer then print_space() else break();
            loop rest
      in
      pr langle_glyph;
      obox0 ();
      loop tms;
      pr rangle_glyph;
      cbox()
  | t -> pr "("; printtm_Term outer ctx t; pr ")"

let printtm ctx t = printtm_Term true ctx t

let prresult ctx t tyT =
  printtm_ATerm true ctx t;
  print_break 1 2;
  pr ": ";
  printty ctx tyT

let prbinding ctx b = match b with
    NameBind -> ()
  | TyVarBind knK -> pr ":: "; printkn ctx knK
  | VarBind tyT -> pr ": "; printty ctx tyT
  | TyVarDef(_tyT, None) -> ()
  | TyVarDef(_tyT, Some knK) -> pr ":: "; printkn ctx knK
  | VarDef(_t, None) -> ()
  | VarDef(_t, Some tyT) -> pr ": "; printty ctx tyT
