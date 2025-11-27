main =
  let {
    swap =
      \p .
        case p {
          (x, y) => (y, x)
        };

    p = (1, 2)
  }
    swap p
  end
