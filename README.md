Benjamin Pierce's F-omega checker, from

  http://www.cis.upenn.edu/~bcpierce/tapl/checkers/fullomega/

Available online at

  http://ocamllabs.github.io/fomega/

## Syntax

There are two types of variables.  Term variables begin with a lowercase letter

    a, b, c
    x, y, z

Type variables begin with an uppercase or Greek letter

    A, B, C
    α, β, γ

A program is a sequence of definitions, terminated by semicolons:

    program ::= definition ; definition ; ... definition ;

Definitions bind types or terms.

    definition ::= α :: kind = type
                   x : type = term

You can leave out either the ` :: kind` or the `= type` component (but not both) from a type binding, and you can leave out either the `: type` or the `= term` component (but not both) from a term binding.

Kinds are either `*`, the type of kinds, or arrow kinds.

    kind ::= *
             kind ⇒ kind
             ( kind )

ASCII alternative: you can use `=>` in place of `⇒`.

Types are defined as follows:

    type ::= type → type             (function types)
             type × type             (product types)
             type + type             (sum types)
             ∀ α :: kind . type      (polymorphic types)
             ∃ α :: kind . type      (existential types)
             λ α :: kind . type      (type abstractions)
             type type               (type applications)
             ( type )

ASCII alternatives: `->` for `→`, `*` for `×`, `All` for `∀`, `EXISTS` for `∃`, `lambda` for `λ`.  Kind annotations (`:: kind`) can be omitted, and default to `*`.

Terms are defined as follows:

    term ::= λ x : type . term
            term term
            ⟨ term , term , ... term ⟩
            @n term                           (nth projection: @1 is fst, @2 is snd)
            inl [ type ] term
            inr [ type ] term
            case term of x.term | y . term
            Λ α :: kind . term
            term [ type ]
            pack type , term, as ∃ α :: kind . type
            open term as α , x in term
            ( term )
             
ASCII alternatives: `lambda` for `λ`, `LAMBDA` for `Λ`.  Kind annotations (`:: kind`) can be omitted, and default to `*`.

## Web version

You can run the web version by using: `dune exec src/runweb.exe`.
