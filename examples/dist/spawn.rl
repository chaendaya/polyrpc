main : Int =
  let {
    client : Loc = spawn();
    f = \client : n . n
  }
    f 3
  end