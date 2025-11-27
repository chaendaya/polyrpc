data Pair a b = Pair a b ;

main =
  let {
    sumPair =
      \p .
        case p {
          Pair x y => x + y
        };

    p = Pair 1 2
  }
    sumPair p
  end
