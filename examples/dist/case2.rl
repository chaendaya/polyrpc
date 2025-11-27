main =
  let {
    s = read ();
    t = (s, " <- you typed\n");
    printPair =
      \p .
        case p {
          (x, y) => print (concat x y)
        }
  }
    printPair t
  end
