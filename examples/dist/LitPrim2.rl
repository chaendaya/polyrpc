main =
  let {
    a = 1 < 2;
    b = 3 == 4;
    c = True and a;
    d = c or b
  }
    if d then 1 else 0
  end
