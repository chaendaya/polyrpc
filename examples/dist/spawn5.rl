a = spawn();

b = spawn( 
      f = \x. x ;
      g = \y. 
            let { 
                d = spawn();
                h = \z. y + z
            }
                h 1
            end
    );

main = g 3

