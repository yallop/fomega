
Sum = λA::*.λB::*.∀C::*.(A -> C) -> (B -> C) -> C;

_inl = ΛA::*.ΛB::*.λx:A.ΛC::*.λl:A->C.λr:B->C.l x;

_inr = ΛA::*.ΛB::*.λy:B.ΛC::*.λl:A->C.λr:B->C.r y;

_case = ΛA::*.ΛB::*.λs:Sum A B.s;

bimap = ΛA::*.ΛB::*.ΛC::*.λf:A->C.λg:B->C.λs:Sum A B.
        _case [A] [B] s [C]
          (λleft:A. f left)
          (λright:B. g right)
          ;

swap = ΛA::*.ΛB::*.λs:Sum A B.
        _case [A] [B] s [Sum B A]
          (λleft:A. _inr [B] [A] left)
          (λright:B. _inl [B] [A] right)
;

proj = ΛA::*.ΛB::*.λs:Sum A A.
        _case [A] [A] s [A]
          (λleft:A. left)
          (λright:A. right)
;
