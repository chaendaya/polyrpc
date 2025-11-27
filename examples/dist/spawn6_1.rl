a = spawn();

b = spawn( 
      f = \x. x ;
      g = \y. 
            let { 
                d = spawn( h = \z. z )
            }
                h (f 1)
            end
    );

main = h 3