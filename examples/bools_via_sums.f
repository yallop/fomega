Unit = ∀A::*.A -> A;

Bool = Unit + Unit;

if = λb:Bool.(ΛA::*.(λr:A.(λs:A.(case b of x . s | y . r))));

unit = ΛA.λa:A.a;

true = inr [Unit] unit; 

false = inl [Unit] unit; 

not = λb:Bool.if b [Bool] false true;

and = λx:Bool.λy:Bool.if x [Bool] y false;

or = λx:Bool.λy:Bool.if x [Bool] true y;
