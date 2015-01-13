Bool = ∀α::*.α → α → α;

true = Λα::*.λt:α.λf:α.t;

false = Λα::*.λt:α.λf:α.f;

if = λb:Bool.b;

not = λb:Bool.if b [Bool] false true;

and = λx:Bool.λy:Bool.x [Bool] y false;

or = λx:Bool.λy:Bool.x [Bool] true y;
