main =
  let {
    a = 1;
    b = 2;
    sa = intToString a;
    sb = intToString b;
    msg = concat "a + b = " (concat sa (concat " + " (concat sb "\n")))
  }
    print msg
  end
