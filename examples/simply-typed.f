Base :: *;

id = λx:Base.x;

idf = λx:Base->Base.x;

compose = λf:Base->Base.λg:Base->Base.λx:Base.f (g x);
