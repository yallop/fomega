open Format
open Syntax
open Support.Error
open Support.Pervasive

(* ------------------------   EVALUATION  ------------------------ *)

(* Treat variable bindings as values. We also treat "constant"
   destructions of such bindings as values. They are constant in the
   sense that it would be safe to perform them lazily even if the
   language had references. *)
let rec isconst ctx t = match t with
  | TmProj(_, c, _) -> isconst ctx c
  | TmTApp(_, c, _) -> isconst ctx c
  | TmOpe (_, c1, _, _, c2) -> isconst ctx c1 && isconst ctx c2
  | TmCase(_, c1, _, c2, _, c3) ->
      isconst ctx c1 && isconst ctx c2 && isconst ctx c3
  | TmVar(fi,x,_) -> isconstbinding fi ctx x
  | _ -> false

and isconstbinding fi ctx x =
  match getbinding fi ctx x with
  | VarBind _ -> true
  | _ -> false

let rec isval ctx t = match t with
    TmAbs(_,_,_,_) -> true
  | TmTAbs(_,_,_,_) -> true
  | TmProd(_,tms) when List.for_all (isval ctx) tms -> true
  | TmPac(_,_,v1,_,_,_) when isval ctx v1 -> true
  | TmInj(_, _, _, v) when isval ctx v -> true
  | TmVar(fi, x, _) when isvalbinding fi ctx x -> true
  | t -> isconst ctx t

and isvalbinding fi ctx x =
  match getbinding fi ctx x with
  | VarDef(t, _) -> isval ctx t
  | _ -> false

let rec expand ctx t = match t with
  | TmVar(fi, n, _) -> begin
      match getbinding fi ctx n with
      | VarDef(t,_) -> expand ctx t
      | _ -> t
    end
  | _ -> t

exception NoRuleApplies

let rec eval1 ctx t = match t with
  | TmApp(_fi,v1 ,v2) when isval ctx v1 && isval ctx v2 -> begin
      match expand ctx v1 with
      | TmAbs(_,_x,_tyT11,t12) -> termSubstTop v2 t12
      | _ -> raise NoRuleApplies
    end
  | TmApp(fi,v1,t2) when isval ctx v1 ->
      let t2' = eval1 ctx t2 in
      TmApp(fi, v1, t2')
  | TmApp(fi,t1,t2) ->
      let t1' = eval1 ctx t1 in
      TmApp(fi, t1', t2)
  | TmTApp(_fi,v1,tyT2) when isval ctx v1-> begin
      match expand ctx v1 with
      | TmTAbs(_,_x,_,t11) -> tytermSubstTop tyT2 t11
      | _ -> raise NoRuleApplies
    end
  | TmTApp(fi,t1,tyT2) ->
      let t1' = eval1 ctx t1 in
      TmTApp(fi, t1', tyT2)
  | TmPac(fi, ty1, tm, x, knd, ty2) ->
    let tm' = eval1 ctx tm in
    TmPac(fi, ty1, tm', x, knd, ty2)
  | TmOpe (_fi,v1, _, _, t2) when isval ctx v1 -> begin
      match expand ctx v1 with
      | TmPac(_, tyT11, v12, _, _, _) ->
          tytermSubstTop tyT11 (termSubstTop (termShift 1 v12) t2)
      | _ -> raise NoRuleApplies
    end
  | TmOpe (fi,t1, tyX, x, t2) ->
      let t1' = eval1 ctx t1 in
      TmOpe(fi, t1', tyX, x, t2)
  | TmProd(fi,tms) ->
      let rec loop l = match l with
          [] -> raise NoRuleApplies
        | vl::rest when isval ctx vl ->
            let rest' = loop rest in
            vl::rest'
        | tm::rest ->
            let tm' = eval1 ctx tm in
            tm'::rest
      in
      let tms' = loop tms in
        TmProd(fi, tms')
  | TmProj(_fi, v1, i) when isval ctx v1 -> begin
      match expand ctx v1 with
      | TmProd(_, tms) when i <= List.length tms -> List.nth tms (i-1)
      | _ -> raise NoRuleApplies
    end
  | TmProj(fi, t1, l) ->
      let t1' = eval1 ctx t1 in
      TmProj(fi, t1', l)
  | TmCase(_fi, v1, _x, tm1, _y, tm2) when isval ctx v1 -> begin
      match expand ctx v1 with
      | TmInj(_, _, L, v) -> termSubstTop v tm1
      | TmInj(_, _, R, v) -> termSubstTop v tm2
      | _ -> raise NoRuleApplies
    end
  | TmCase(fi,tm,x,tm1,y,tm2) ->
      let tm' = eval1 ctx tm in
      TmCase(fi,tm',x,tm1,y,tm2)
  | TmInj(fi, ty, i, tm) ->
      let tm' = eval1 ctx tm in
      TmInj (fi, ty, i, tm')
  | TmVar(fi, n, _) when isvalbinding fi ctx n ->
      raise NoRuleApplies
  | TmVar(fi, n, _) -> begin
      match getbinding fi ctx n with
      | VarDef(t,_) -> t
      | _ -> raise NoRuleApplies
    end
  | _ ->
      raise NoRuleApplies

let rec eval ctx t =
  try let t' = eval1 ctx t
      in eval ctx t'
  with NoRuleApplies -> t

(* ------------------------   KINDING  ------------------------ *)

let istydefined ctx i =
  match getbinding dummyinfo ctx i with
  | TyVarDef(_tyT, _) -> true
  | _ -> false

let gettydef ctx i =
  match getbinding dummyinfo ctx i with
    TyVarDef(tyT,_) -> tyT
  | _ -> raise NoRuleApplies

let computety ctx tyT = match tyT with
  | TyVar(i, _) when istydefined ctx i -> gettydef ctx i
  | TyApp(TyAbs(_,_,tyT12),tyT2) -> typeSubstTop tyT2 tyT12
  | _ -> raise NoRuleApplies

let rec simplifyty ctx tyT =
  let tyT =
    match tyT with
        TyApp(tyT1,tyT2) -> TyApp(simplifyty ctx tyT1,tyT2)
      | tyT -> tyT
  in
  try
    let tyT' = computety ctx tyT in
    simplifyty ctx tyT'
  with NoRuleApplies -> tyT

let kindeqv _ctx knK knK' = (knK = knK')

let rec tyeqv ctx tyS tyT =
  let tyS = simplifyty ctx tyS in
  let tyT = simplifyty ctx tyT in
  match (tyS,tyT) with
  | (TyVar(i,_), _) when istydefined ctx i ->
      tyeqv ctx (gettydef ctx i) tyT
  | (_, TyVar(i,_)) when istydefined ctx i ->
      tyeqv ctx tyS (gettydef ctx i)
  | (TyVar(i,_),TyVar(j,_)) -> i=j
  | (TyAll(tyX1,knK1,tyS2),TyAll(_,knK2,tyT2)) ->
       let ctx1 = addname ctx tyX1 in
       (kindeqv ctx knK1 knK2) && (tyeqv ctx1 tyS2 tyT2)
  | (TyExi(tyX1,knK1,tyS2),TyExi(_,knK2,tyT2)) ->
       let ctx1 = addname ctx tyX1 in
       (kindeqv ctx knK1 knK2) && (tyeqv ctx1 tyS2 tyT2)
  | (TyAbs(tyX1,knKS1,tyS2),TyAbs(_,knKT1,tyT2)) ->
       let ctx1 = addname ctx tyX1 in
       (kindeqv ctx knKS1 knKT1) && (tyeqv ctx1 tyS2 tyT2)
  | (TyApp(tyS1,tyS2),TyApp(tyT1,tyT2)) ->
       (tyeqv ctx tyS1 tyT1) && (tyeqv ctx tyS2 tyT2)
  | (TyArr(tyS1,tyS2),TyArr(tyT1,tyT2)) ->
       (tyeqv ctx tyS1 tyT1) && (tyeqv ctx tyS2 tyT2)
  | (TyProd(tms1),TyProd(tms2)) ->
      List.length tms1 = List.length tms2
      && List.for_all2
           (fun ty1 ty2-> tyeqv ctx ty1 ty2)
           tms1 tms2
  | (TySum(tyS1,tyS2),TySum(tyT1,tyT2)) ->
       (tyeqv ctx tyS1 tyT1) && (tyeqv ctx tyS2 tyT2)
  | _ -> false

let rec kindof ctx tyT = match tyT with
    TyVar(i,_) ->
      let knK = getKindFromContext dummyinfo ctx i
      in knK
  | TyAll(tyX,knK1,tyT2) ->
      let ctx' = addbinding ctx tyX (TyVarBind knK1) in
      if kindof ctx' tyT2 <> KnStar then error dummyinfo "kind * expected";
      KnStar
  | TyExi(tyX,knK1,tyT2) ->
      let ctx' = addbinding ctx tyX (TyVarBind knK1) in
      if kindof ctx' tyT2 <> KnStar then error dummyinfo "kind * expected";
      KnStar
  | TyAbs(tyX,knK1,tyT2) ->
      let ctx' = addbinding ctx tyX (TyVarBind(knK1)) in
      let knK2 = kindof ctx' tyT2 in
      KnArr(knK1,knK2)
  | TyApp(tyT1,tyT2) ->
      let knK1 = kindof ctx tyT1 in
      let knK2 = kindof ctx tyT2 in
      (match knK1 with
          KnArr(knK11,knK12) ->
            if (=) knK2 knK11 then knK12
            else error dummyinfo "parameter kind mismatch"
        | _ -> error dummyinfo "arrow kind expected")
  | TyArr(tyT1,tyT2) ->
      if kindof ctx tyT1 <> KnStar then error dummyinfo "kind * expected";
      if kindof ctx tyT2 <> KnStar then error dummyinfo "kind * expected";
      KnStar
  | TyProd(tys) ->
      List.iter
        (fun ty ->
           if kindof ctx ty <> KnStar then
             error dummyinfo "kind * expected")
        tys;
      KnStar
  | TySum(tyT1,tyT2) ->
      if kindof ctx tyT1 <> KnStar then error dummyinfo "kind * expected";
      if kindof ctx tyT2 <> KnStar then error dummyinfo "kind * expected";
      KnStar


let checkkindstar fi ctx tyT =
  let k = kindof ctx tyT in
  if k = KnStar then ()
  else error fi "kind * expected"

(* ------------------------   TYPING  ------------------------ *)

let rec typeof ctx t =
  match t with
    TmVar(fi,i,_) -> getTypeFromContext fi ctx i
  | TmAbs(fi,x,tyT1,t2) ->
      checkkindstar fi ctx tyT1;
      let ctx' = addbinding ctx x (VarBind(tyT1)) in
      let tyT2 = typeof ctx' t2 in
      TyArr(tyT1, typeShift (-1) tyT2)
  | TmApp(_fi,t1,t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match simplifyty ctx tyT1 with
          TyArr(tyT11,tyT12) ->
            if tyeqv ctx tyT2 tyT11 then tyT12
            else
              errfAt (tmInfo t2) (fun () ->
                  pr "Parameter type mismatch:";
                  force_newline ();
                  pr "This term has type ";
                  printty ctx (simplifyty ctx tyT2);
                  pr " instead of ";
                  printty ctx (simplifyty ctx tyT11);
                  force_newline ())
       | _ -> errfAt (tmInfo t1) (fun () ->
           pr "Arrow type expected:";
           force_newline ();
           printty ctx tyT1;
           pr " is not an arrow type, it cannot be applied.";
           force_newline ())
      )
  | TmTAbs(_fi,tyX,knK1,t2) ->
      let ctx = addbinding ctx tyX (TyVarBind(knK1)) in
      let tyT2 = typeof ctx t2 in
      TyAll(tyX,knK1,tyT2)
  | TmTApp(fi,t1,tyT2) ->
      let knKT2 = kindof ctx tyT2 in
      let tyT1 = typeof ctx t1 in
      (match simplifyty ctx tyT1 with
           TyAll(_,knK11,tyT12) ->
             if knK11 <> knKT2 then
               error fi "Type argument has wrong kind";
             typeSubstTop tyT2 tyT12
         | _ -> error fi "Universal type expected")
  | TmPac(fi,ty1,tm,x,k,ty2) ->
      let knTy1 = kindof ctx ty1 in
      let tyTm = typeof ctx tm in
      if knTy1 <> k then
        error fi "Pack kind mismatch";
      let subst = typeSubstTop ty1 ty2 in
      if not (tyeqv ctx subst tyTm) then
        error fi "Pack type mismatch"
      else
        TyExi (x, k, ty2)
  | TmOpe(fi,t1,x,y,t2) ->
      let tyT1 = typeof ctx t1 in
      (match simplifyty ctx tyT1 with
         TyExi(_,kn,t3) ->
           let ctx = addbinding ctx x (TyVarBind kn) in
           let ctx = addbinding ctx y (VarBind t3) in
           typeShift (-2) (typeof ctx t2)
       | _ -> error fi "Existential type expected")
  | TmProd(_fi, tms) ->
      let tys = List.map (typeof ctx) tms in
      TyProd(tys)
  | TmProj(fi, t1, i) ->
      (match simplifyty ctx (typeof ctx t1) with
          TyProd(tys) ->
            if i <= List.length tys then
              List.nth tys (i-1)
            else error fi ("Element "^(string_of_int i)^" not found")
        | _ -> error fi "Product type expected")
  | TmCase(fi,tm1,x,tm2,y,tm3) ->
      (match simplifyty ctx (typeof ctx tm1) with
          TySum(tyL,tyR) ->
           let ltype =
             let ctx' = addbinding ctx x (VarBind tyL) in
             typeShift (-1) (typeof ctx' tm2)
           and rtype =
             let ctx' = addbinding ctx y (VarBind tyR) in
             typeShift (-1) (typeof ctx' tm3)
           in
           if not (tyeqv ctx ltype rtype)
           then error fi "Branches do not have the same type"
           else ltype
        | _ -> error fi "Variant type expected")
  | TmInj(_fi,ty,L,t) ->
    TySum(simplifyty ctx (typeof ctx t),
          simplifyty ctx ty)
  | TmInj(_fi,ty,R,t) ->
    TySum(simplifyty ctx ty,
          simplifyty ctx (typeof ctx t))

(* -------------------- Prettyfy -------------------------- *)

(* Simplify type applications within expressions for better printing *)

let ap1 g x f =
  match g x with
  | None -> None
  | Some x -> Some (f x)
let ap2 g x y f =
  match g x, g y with
  | None, None -> None
  | Some x, None -> Some (f x y)
  | None, Some y -> Some (f x y)
  | Some x, Some y -> Some (f x y)
let apn g xs f =
  let one x (xs,changed) =
    match g x with
      None -> (x :: xs), changed
    | Some x -> (x :: xs), true
  in
  (* Return a list*bool pair: mapped elements, whether anything changed *)
  match List.fold_right one xs ([], false) with
    _, false -> None
  | xs, true -> Some (f xs)

let rec prettifyty' tyT = match tyT with
  | TyApp(TyAbs(_,_,tyT12),tyT2) -> Some (typeSubstTop tyT2 tyT12)
  | TyVar _ -> None
  | TyArr(ty1, ty2) ->
    ap2 prettifyty' ty1 ty2
      (fun ty1 ty2 -> TyArr(ty1, ty2))
  | TyAll(s, k, ty) ->
    ap1 prettifyty' ty (fun ty -> TyAll(s, k, ty))
  | TyExi(s, k, ty) ->
    ap1 prettifyty' ty (fun ty -> TyExi(s, k, ty))
  | TyAbs(s, k, ty) ->
    ap1 prettifyty' ty (fun ty -> TyAbs(s, k, ty))
  | TyApp(ty1, ty2) ->
    ap2 prettifyty' ty1 ty2 (fun ty1 ty2 -> TyApp(ty1, ty2))
  | TyProd tyl ->
    apn prettifyty' tyl (fun tyl -> TyProd tyl)

  | TySum(ty1, ty2) ->
    ap2 prettifyty' ty1 ty2 (fun ty1 ty2 -> TySum(ty1, ty2))

let rec prettifyty tyT =
  match prettifyty' tyT with
    None -> tyT
  | Some tyT -> prettifyty tyT

(*
let rec prettifyty tyT = match tyT with
  | TyApp(TyAbs(_,_,tyT12),tyT2) -> typeSubstTop tyT2 tyT12
  | TyVar _ -> tyT
  | TyArr(ty1, ty2) -> TyArr(prettifyty ty1, prettifyty ty2)
  | TyAll(s, k, ty) -> TyAll(s, k, prettifyty ty)
  | TyExi(s, k, ty) -> TyExi(s, k, prettifyty ty)
  | TyAbs(s, k, ty) -> TyAbs(s, k, prettifyty ty)
  | TyApp(ty1, ty2) -> TyApp(prettifyty ty1, prettifyty ty2)
  | TyProd tyl -> TyProd (List.map prettifyty tyl)
  | TySum(ty1, ty2) -> TySum(prettifyty ty1, prettifyty ty2)
*)

let rec prettify t = match t with
  | TmVar _ -> t
  | TmAbs(fi, s, ty, t) ->
      TmAbs(fi, s, prettifyty ty, prettify t)
  | TmApp(fi, t1, t2) ->
      TmApp(fi, prettify t1, prettify t2)
  | TmPac(fi, ty1, t, s, kn, ty2) ->
      TmPac(fi, prettifyty ty1, prettify t, s, kn, prettifyty ty2)
  | TmOpe(fi, t1, s1, s2, t2) ->
      TmOpe(fi, prettify t1, s1, s2, prettify t2)
  | TmTAbs(fi, s, kn, t) ->
      TmTAbs(fi, s, kn, prettify t)
  | TmTApp(fi, t, ty) ->
      TmTApp(fi, prettify t, prettifyty ty)
  | TmProd(fi, ts) ->
      TmProd(fi, ts)
  | TmProj(fi, t, i) ->
      TmProj(fi, prettify t, i)
  | TmCase(fi, t1, s1, t2, s2, t3) ->
      TmCase(fi, prettify t1, s1, prettify t2, s2, prettify t3)
  | TmInj(fi, ty, inj, term) ->
      TmInj(fi, prettifyty ty, inj, term)

let prettifybinding bind = match bind with
  | NameBind -> bind
  | TyVarBind _ -> bind
  | VarBind ty -> VarBind (prettifyty ty)
  | TyVarDef(ty, knd_opt) -> TyVarDef(prettifyty ty, knd_opt)
  | VarDef(t, None) -> VarDef(prettify t, None)
  | VarDef(t, Some ty) -> VarDef(prettify t, Some (prettifyty ty))

(* -------------------- Top-level bindings ----------------- *)

let checkbinding fi ctx b = match b with
  | NameBind -> NameBind
  | TyVarBind _knK -> b
  | VarBind _tyT -> b
  | TyVarDef(tyT, None) -> TyVarDef(tyT, Some (kindof ctx tyT))
  | TyVarDef(tyT, Some knK) ->
      let knK' = kindof ctx tyT in
      if kindeqv ctx knK knK' then b
      else error fi "kind of binding does not match declared kind"
  | VarDef(t, None) -> VarDef(t, Some(typeof ctx t))
  | VarDef(t, Some tyT) ->
     let tyT' = typeof ctx t in
     if tyeqv ctx tyT' tyT then b
     else error fi "type of binding does not match declared type"

let evalbinding ctx b = match b with
  | VarDef(t,tyT) ->
      let t' = eval ctx t in
      VarDef(t',tyT)
  | bind -> bind

(* -------------------- Top-level open --------------------- *)

let typeopen fi ctx t =
  let tyT = typeof ctx t in
    match simplifyty ctx tyT with
    | TyExi(_,kn,ty) -> kn, ty
    | _ -> error fi "existential type expected"

let evalopen ctx t =
  let t = eval ctx t in
    match expand ctx t with
    | TmPac(_, _, t1, _, _, _) -> Some (termShift 1 t1)
    | _ -> None
