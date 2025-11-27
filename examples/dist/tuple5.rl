main =
  let {
    showPair =
      \p .
        case p {
          (x, y) =>
            concat
              (concat "pair = (" (intToString x))
              (concat ", " (concat (intToString y) ")\n"))
        };

    p = (10, 20)
  }
    print (showPair p)
  end
