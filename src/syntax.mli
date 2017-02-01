(* module Syntax: syntax trees and associated support functions *)

open Support.Pervasive
open Support.Error

(* Data type definitions *)
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
  | TmMagic of info * ty * term

type binding =
    NameBind
  | TyVarBind of kind
  | VarBind of ty
  | TyVarDef of ty * kind option
  | VarDef of term * ty option

type command =
  | Eval of info * term
  | Bind of info * string * binding
  | Open of info * term * string * string

(* Contexts *)
type context
val emptycontext : context
val ctxlength : context -> int
val addbinding : context -> string -> binding -> context
val addname: context -> string -> context
val index2name : info -> context -> int -> string
val getbinding : info -> context -> int -> binding
val name2index : info -> context -> string -> int
val isnamebound : context -> string -> bool
val getTypeFromContext : info -> context -> int -> ty
val getKindFromContext : info -> context -> int -> kind

(* Shifting and substitution *)
val termShift: int -> term -> term
val termSubstTop: term -> term -> term
val typeShift : int -> ty -> ty
val typeSubstTop: ty -> ty -> ty
val tytermSubstTop: ty -> term -> term

(* Printing *)
val printtm: context -> term -> unit
val printtm_ATerm: bool -> context -> term -> unit
val printty : context -> ty -> unit
val printkn : context -> kind -> unit
val prresult : context -> term -> ty -> unit
val prbinding : context -> binding -> unit

(* Misc *)
val tmInfo: term -> info
