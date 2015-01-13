# Embedding of simply-typed lambda calculus using higher-order abstract syntax.

# The dictionary for lambda terms
ExpDict = λE::*⇒*.∀A::*.
         ((∀A::*.∀B::*.(E A → E B) → E (A → B)) →
         (∀A::*.∀B::*.E (A → B) → (E A → E B)) →
         A) → A;


# term constructors
# abs : ∀A::*.∀B::*.∀E::*⇒*.ExpDict E → (E A → E B) → E (A → B)
abs = ΛA::*.ΛB::*.ΛE::*⇒*.λdict:ExpDict E.λf:E A → E B.
      dict  [∀A::*.∀B::*.(E A → E B) → E (A → B)]
        (λabs:∀A::*.∀B::*.(E A → E B) → E (A → B).
         λapp:∀A::*.∀B::*.E (A → B) → (E A → E B).
         abs) [A] [B] f
;

# app : ∀A::*.∀B::*.Exp (A → B) → (Exp A → Exp B)
app = ΛA::*.ΛB::*.ΛE::*⇒*.λdict:ExpDict E.λf:E (A → B).λp:E A.
      dict  [∀A::*.∀B::*.E (A → B) → (E A → E B)]
        (λabs:∀A::*.∀B::*.(E A → E B) → E (A → B).
         λapp:∀A::*.∀B::*.E (A → B) → (E A → E B).
         app) [A] [B] f p
;

# A type of unevaluated expressions
Expression = λA::*.(∀E::*⇒*.ExpDict E → E A);

# Evaluator: interpret lambda terms as functions

# evalDict : ExpDict (λA::*.A)
evalDict = ΛA::*.
     λk:(∀A::*.∀B::*.(A → B) → (A → B)) →
        (∀A::*.∀B::*.(A → B) → (A → B)) →
        A.
        k (ΛA::*.ΛB::*.λf:A→B.f) (ΛA::*.ΛB::*.λf:A→B.f)
;
      
# eval : ∀A::*.Expression A → A
eval = ΛA::*.λe:Expression A.e [λA::*.A] evalDict;

# natural numbers
Nat = ∀A::*.A → (A → A) → A;

# succ : Nat
zero = ΛA::*.λz:A.λs:A → A.z;

# succ : Nat → Nat
succ = λn:Nat.ΛA::*.λz:A.λs:A → A.s (n [A] z s);

# add : Nat → Nat → Nat
add = λm:Nat.λn:Nat.m [Nat] n succ;

# Term size measurer: interpret lambda terms as integers representing their size
# sizeDict : ExpDict (λA::*.Nat)
sizeDict = ΛA::*.
     λk:(∀A::*.∀B::*.(Nat → Nat) → Nat) →
        (∀A::*.∀B::*.Nat → Nat → Nat) →
        A.
        k (ΛA::*.ΛB::*.λf:Nat → Nat.succ (f (succ zero)))
          (ΛA::*.ΛB::*.λf:Nat.λp:Nat.add f p)
;

# size : ∀A::*.Expression A → Nat
size = ΛA::*.λe:Expression A. e [λA::*.Nat] sizeDict;
