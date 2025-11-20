main : Int =
  let {
    client : Loc = spawn();

    b : Loc = spawn( { 
      f = \x. x ;
      g = \y. y + 1
    });

    fun1 = \client : n . f n
  }
    f 3
  end