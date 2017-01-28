# Embedding of simply-typed lambda calculus using higher-order abstract syntax.

# The dictionary for lambda terms
ExpDict = λE::*⇒*.∀A::*.
         ((∀A::*.∀B::*.(E A → E B) → E (A → B)) →
         (∀A::*.∀B::*.E (A → B) → (E A → E B)) →
         A) → A;


# term constructors
abs : ∀A::*.∀B::*.∀E::*⇒*.ExpDict E → (E A → E B) → E (A → B)
  = ΛA::*.ΛB::*.ΛE::*⇒*.λdict:ExpDict E.λf:E A → E B.
      dict  [∀A::*.∀B::*.(E A → E B) → E (A → B)]
        (λabs:∀A::*.∀B::*.(E A → E B) → E (A → B).
         λapp:∀A::*.∀B::*.E (A → B) → (E A → E B).
         abs) [A] [B] f
;

app : ∀A.∀B.∀E::*⇒*.ExpDict E → E (A → B) → E A → E B
  = ΛA::*.ΛB::*.ΛE::*⇒*.λdict:ExpDict E.λf:E (A → B).λp:E A.
      dict  [∀A::*.∀B::*.E (A → B) → (E A → E B)]
        (λabs:∀A::*.∀B::*.(E A → E B) → E (A → B).
         λapp:∀A::*.∀B::*.E (A → B) → (E A → E B).
         app) [A] [B] f p
;

# A type of unevaluated expressions
Expression = λA::*.(∀E::*⇒*.ExpDict E → E A);

# Evaluator: interpret lambda terms as functions

evalDict : ExpDict (λA::*.A)
  = ΛA::*.
     λk:(∀A::*.∀B::*.(A → B) → (A → B)) →
        (∀A::*.∀B::*.(A → B) → (A → B)) →
        A.
        k (ΛA::*.ΛB::*.λf:A→B.f) (ΛA::*.ΛB::*.λf:A→B.f)
;
      
eval : ∀A::*.Expression A → A
  = ΛA::*.λe:Expression A.e [λA::*.A] evalDict;

# natural numbers
Nat = ∀A::*.A → (A → A) → A;

zero : Nat
  = ΛA::*.λz:A.λs:A → A.z;

succ : Nat → Nat
  = λn:Nat.ΛA::*.λz:A.λs:A → A.s (n [A] z s);

add : Nat → Nat → Nat
  = λm:Nat.λn:Nat.m [Nat] n succ;

# Term size measurer: interpret lambda terms as integers representing their size
sizeDict : ExpDict (λA::*.Nat)
  = ΛA::*.
     λk:(∀A::*.∀B::*.(Nat → Nat) → Nat) →
        (∀A::*.∀B::*.Nat → Nat → Nat) →
        A.
        k (ΛA::*.ΛB::*.λf:Nat → Nat.succ (f (succ zero)))
          (ΛA::*.ΛB::*.λf:Nat.λp:Nat.add f p)
;

size : ∀A::*.Expression A → Nat
  = ΛA::*.λe:Expression A. e [λA::*.Nat] sizeDict;
