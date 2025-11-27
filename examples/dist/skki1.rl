a = spawn ();
b = spawn ();
c = spawn ();

s = \a : f  g  x. f x (g x);
k = \b : x  y. x;
identity = \c : x. s k k x ;

main = identity 123