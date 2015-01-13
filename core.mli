(* module Core

   Core typechecking and evaluation functions
*)

open Syntax
open Support.Error

val eval : context -> term -> term
val typeof : context -> term -> ty
val kindof : context -> ty -> kind
val tyeqv : context -> ty -> ty -> bool
val simplifyty : context -> ty -> ty

val prettifyty : ty -> ty
val prettify : term -> term
val prettifybinding : binding -> binding

val checkbinding : info -> context -> binding -> binding
val evalbinding : context -> binding -> binding

val typeopen : info -> context -> term -> kind * ty
val evalopen : context -> term -> term option
