a = spawn (s = \f  g  x. f x (g x));
b = spawn (k = \x  y. x);
c = spawn (identity = \x. s k k x);

main = identity 123