f = \loc. \loc : x . x+1 ;

a = spawn();
b = spawn();

main = (f a 1) + (f b 1)