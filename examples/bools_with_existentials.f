Unit = ∀A::*.A -> A;

unit = ΛA.λa:A.a;

# an abstract type of bools, packaged as an existential
# there are three elements representing 'true', 'false', 'if'
bool_package =
  pack (Unit+Unit),
       ⟨inr [Unit] unit,
       ⟨inl [Unit] unit,
        λb:Unit+Unit.
          Λα.
           λr:α.
            λs:α.
             case b of x.s | y.r ⟩⟩
   as
     ∃ β.(β ×                  # true
         (β ×                  # false
         (β -> (∀α.α → α → α)) # if
         ))
;

# simple example of using the abstract bool type
# open the package and evaluate a term corresponding to the following:
#   if true then inl () else inr ()
open bool_package as β, b in
  (@2 (@2 b))         # if
    (@1 b)            # true
    [Unit + Unit]     # (result type)
    (inl [Unit] unit) # "then" expression
    (inr [Unit] unit) # "else" expression
;
