# Eq : * ⇒ * ⇒ *;
Eq = λA::*.λB::*.∀P::* ⇒ *.P A → P B;

# refl : ∀A::*.eq A A;
refl = ΛA::*.ΛP::*⇒*.λx:P A.x;

# symm : ∀A::*.∀B::*.eq A B → Eq B A;
symm = ΛA::*.ΛB::*.λe:(∀P::* ⇒ *.P A → P B).e [λX::*.Eq X A] (refl [A]);

# trans : ∀A::*.∀B::*.∀C::*.Eq A B → Eq B C → Eq A C;
trans = ΛA::*.ΛB::*.ΛC::*.λab:Eq A B.λbc:Eq B C. bc [Eq A] ab;

# lift : ∀A::*.∀B::*.∀P::* ⇒ *.Eq A B → Eq (P A) (P B);
lift = ΛA::*.ΛB::*.ΛP::* ⇒ *.λe:Eq A B.e [λX::*.Eq (P A) (P X)] (refl [P A]);

