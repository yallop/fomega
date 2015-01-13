
Pair = λA::*.λB::*.∀C::*.(A -> B -> C) -> C;

pair = ΛA::*.ΛB::*.λx:A.λy:B.ΛC::*.λk:(A -> B -> C).k x y;

fst = ΛA::*.ΛB::*.λp:Pair A B.p [A] (λx:A.λy:B.x);

snd = ΛA::*.ΛB::*.λp:Pair A B.p [B] (λx:A.λy:B.y);
