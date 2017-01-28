(*
 *  Yacc grammar for the parser.  The files parser.mli and parser.ml
 *  are generated automatically from parser.mly.
 *)

%{
open Support.Error
open Support.Pervasive
open Syntax
%}

(* ---------------------------------------------------------------------- *)
(* Preliminaries *)

(* We first list all the tokens mentioned in the parsing rules
   below.  The names of the tokens are common to the parser and the
   generated lexical analyzer.  Each token is annotated with the type
   of data that it carries; normally, this is just file information
   (which is used by the parser to annotate the abstract syntax trees
   that it constructs), but sometimes -- in the case of identifiers and
   constant values -- more information is provided.
 *)

(* Keyword tokens *)
%token <Support.Error.info> LAMBDA
%token <Support.Error.info> BIGLAMBDA
%token <Support.Error.info> ALL
%token <Support.Error.info> EXISTS
%token <Support.Error.info> PACK
%token <Support.Error.info> OPEN
%token <Support.Error.info> CASE
%token <Support.Error.info> INL
%token <Support.Error.info> INR
%token <Support.Error.info> OF
%token <Support.Error.info> AS
%token <Support.Error.info> IN

(* Identifier and constant value tokens *)
%token <string Support.Error.withinfo> UCID  (* uppercase-initial *)
%token <string Support.Error.withinfo> LCID  (* lowercase/symbolic-initial *)
%token <int Support.Error.withinfo> INDEX  (* index *)
%token <int Support.Error.withinfo> SUBINDEX  (* subscripted index *)

(* Symbolic tokens *)
%token <Support.Error.info> ARROW
%token <Support.Error.info> COLON
%token <Support.Error.info> COLONCOLON
%token <Support.Error.info> COMMA
%token <Support.Error.info> DARROW
%token <Support.Error.info> DOT
%token <Support.Error.info> EOF
%token <Support.Error.info> EQ
%token <Support.Error.info> VBAR
%token <Support.Error.info> LPAREN
%token <Support.Error.info> LSQUARE
%token <Support.Error.info> RPAREN
%token <Support.Error.info> RSQUARE
%token <Support.Error.info> SEMI
%token <Support.Error.info> STAR
%token <Support.Error.info> USCORE
%token <Support.Error.info> LANGLE
%token <Support.Error.info> RANGLE
%token <Support.Error.info> TIMES
%token <Support.Error.info> PLUS
%token <Support.Error.info> AT
%token <Support.Error.info> PI

(* ---------------------------------------------------------------------- *)
(* The starting production of the generated parser is the syntactic class
   toplevel.  The type that is returned when a toplevel is recognized is
     Syntax.context -> (Syntax.command list * Syntax.context)
   that is, the parser returns to the user program a function that,
   when given a naming context, returns a fully parsed list of
   Syntax.commands and the new naming context that results when
   all the names bound in these commands are defined.

   All of the syntactic productions in the parser follow the same pattern:
   they take a context as argument and return a fully parsed abstract
   syntax tree (and, if they involve any constructs that bind variables
   in some following phrase, a new context).

*)

%start toplevel
%type < Syntax.context -> (Syntax.command list * Syntax.context) > toplevel
%%

(* ---------------------------------------------------------------------- *)
(* Main body of the parser definition *)

(* The top level of a file is a sequence of commands, each terminated
   by a semicolon. *)
toplevel :
    EOF
      { fun ctx -> [],ctx }
  | Command SEMI toplevel
      { fun ctx ->
          let cmd,ctx = $1 ctx in
          let cmds,ctx = $3 ctx in
          cmd::cmds,ctx }

(* A top-level command *)
Command :
  | Term
      { fun ctx -> (let t = $1 ctx in Eval(tmInfo t,t)),ctx }
  | UCID TyBinder
      { fun ctx -> ((Bind($1.i, $1.v, $2 ctx)), addname ctx $1.v) }
  | LCID Binder
      { fun ctx -> ((Bind($1.i,$1.v,$2 ctx)), addname ctx $1.v) }
  | OPEN Term AS UCID COMMA LCID
      { fun ctx ->
          let ctx1 = addname ctx $4.v in
          let ctx2 = addname ctx1 $6.v in
          (Open($1,$2 ctx,$4.v,$6.v), ctx2) }


(* Right-hand sides of top-level bindings *)
Binder :
  | COLON Type
      { fun ctx -> VarBind ($2 ctx)}
  | COLON Type EQ Term
      { fun ctx -> VarDef($4 ctx, Some ($2 ctx))}
  | EQ Term
      { fun ctx -> VarDef($2 ctx, None)}

(* All kind expressions *)
Kind :
    ArrowKind
      { $1 }

ArrowKind :
    AKind DARROW ArrowKind  { fun ctx -> KnArr($1 ctx, $3 ctx) }
  | AKind
           { $1 }

(* All type expressions *)
Type :
    ArrowType
                { $1 }
  | ALL UCID OKind DOT Type
      { fun ctx ->
          let ctx1 = addname ctx $2.v in
          TyAll($2.v,$3 ctx,$5 ctx1) }
  | EXISTS UCID OKind DOT Type
      { fun ctx ->
          let ctx1 = addname ctx $2.v in
          TyExi($2.v,$3 ctx,$5 ctx1) }
  | LAMBDA UCID OKind DOT Type
      { fun ctx ->
          let ctx1 = addname ctx $2.v in
          TyAbs($2.v, $3 ctx, $5 ctx1) }

(* Atomic types are those that never need extra parentheses *)
AType :
    LPAREN Type RPAREN
           { $2 }
  | UCID
      { fun ctx ->
          TyVar(name2index $1.i ctx $1.v, ctxlength ctx) }

TyBinder :
  | (* empty *)
      { fun ctx -> TyVarBind(KnStar) }
  | COLONCOLON Kind
      { fun ctx -> TyVarBind($2 ctx) }
  | COLONCOLON Kind EQ Type
      { fun ctx -> TyVarDef($4 ctx, Some ($2 ctx)) }
  | EQ Type
      { fun ctx -> TyVarDef($2 ctx, None) }

AKind :
    STAR { fun ctx -> KnStar }
  | LPAREN Kind RPAREN  { $2 }

OKind :
  (* empty *)
     { fun ctx -> KnStar}
| COLONCOLON Kind
     { $2 }

(* An "arrow type" is a sequence of product types separated by
   arrows. *)
ArrowType :
    SumType ARROW ArrowType
     { fun ctx -> TyArr($1 ctx, $3 ctx) }
  | SumType
            { $1 }

SumType :
    ProdType PLUS SumType
     { fun ctx -> TySum($1 ctx, $3 ctx) }
  | ProdType
     { $1 }

(* A "product type" is a sequence of atomic types separated by
   times symbols. *)
ProdType :
    Product
     { fun ctx -> TyProd(List.map (fun f -> f ctx) $1) }
  | AppType
     { $1 }

Product :
    AppType TIMES Product
      { $1 :: $3 }
  | AppType TIMES AppType
      { [$1; $3] }

Term :
    AppTerm
      { $1 }
  | LAMBDA LCID COLON Type DOT Term
      { fun ctx ->
          let ctx1 = addname ctx $2.v in
          TmAbs($1, $2.v, $4 ctx, $6 ctx1) }
  | CASE Term OF LCID DOT Term VBAR LCID DOT Term
      { fun ctx ->
          let ctx1 = addname ctx $4.v
          and ctx2 = addname ctx $8.v in
          TmCase($1, $2 ctx, $4.v, $6 ctx1, $8.v, $10 ctx2) }
  | LAMBDA USCORE COLON Type DOT Term
      { fun ctx ->
          let ctx1 = addname ctx "_" in
          TmAbs($1, "_", $4 ctx, $6 ctx1) }
  | BIGLAMBDA UCID OKind DOT Term
      { fun ctx ->
          let ctx1 = addname ctx $2.v in
          TmTAbs($1,$2.v,$3 ctx,$5 ctx1) }
  | PACK Type COMMA Term AS EXISTS UCID OKind DOT Type
      { fun ctx ->
          let ctx1 = addname ctx $7.v in
          TmPac($1,$2 ctx,$4 ctx,$7.v,$8 ctx,$10 ctx1) }
  | OPEN Term AS UCID COMMA LCID IN Term
      { fun ctx ->
          let ctx1 = addname ctx $4.v in
          let ctx2 = addname ctx1 $6.v in
          TmOpe($1,$2 ctx,$4.v,$6.v,$8 ctx2) }

(* A sequence of terms seperated by commas. *)
TermList:
    Term COMMA TermList
      { $1 :: $3 }
  | Term COMMA Term
      { [$1;$3] }

AppTerm :
    ATerm
      { $1 }
  | AppTerm ATerm
      { fun ctx ->
          let e1 = $1 ctx in
          let e2 = $2 ctx in
          TmApp(tmInfo e1,e1,e2) }
  | AppTerm LSQUARE Type RSQUARE
      { fun ctx ->
          let t1 = $1 ctx in
          let t2 = $3 ctx in
          TmTApp(tmInfo t1,t1,t2) }
  | AT INDEX ATerm
      { fun ctx ->
          TmProj($1, $3 ctx, $2.v) }
  | PI SUBINDEX ATerm
      { fun ctx ->
          TmProj($1, $3 ctx, $2.v) }
  | INL LSQUARE Type RSQUARE ATerm
      { fun ctx ->
          TmInj($1, $3 ctx, L, $5 ctx) }
  | INR LSQUARE Type RSQUARE ATerm
      { fun ctx ->
          TmInj($1, $3 ctx, R, $5 ctx) }

(* Atomic terms are ones that never require extra parentheses *)
ATerm :
    LPAREN Term RPAREN
      { $2 }
  | LANGLE TermList RANGLE
      { fun ctx -> TmProd($1 , List.map (fun f -> f ctx) $2) }
  | LCID
      { fun ctx ->
           TmVar($1.i, name2index $1.i ctx $1.v, ctxlength ctx) }


AppType :
    AppType AType { fun ctx -> TyApp($1 ctx,$2 ctx) }
  | AType { $1 }


(*   *)
