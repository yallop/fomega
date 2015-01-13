# Perfect :: * ⇒ *
Perfect = λA::*.∀T::*⇒*.(∀A::*.A → T A) → (∀A::*.T (A × A) → T A) → T A;

# zeroP : ∀A::*.A → Perfect A
zeroP = ΛA::*.λx:A.
          ΛT::*⇒*.λz:∀A::*.A → T A.λs:∀A::*.T (A × A) → T A.z [A] x ;

# succP : ∀A::*.Perfect (A × A) → Perfect A
succP = ΛA::*.λp:Perfect (A × A).
          ΛT::*⇒*.λz:∀A::*.A → T A.λs:(∀B::*.T (B × B) → T B).
            s [A] (p [T] z s) ;
