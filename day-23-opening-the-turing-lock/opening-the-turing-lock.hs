
collatzSequence n =
  let
    collatzSequence' n s
      | n == 1 = reverse s
      | even n = collatzSequence' (n `quot` 2) (n:s)
      | odd n = collatzSequence' (n * 3 + 1) (n:s)
  in collatzSequence' n []

solve = do
  putStrLn "Part 1:"
  putStrLn $ show $ length $ collatzSequence 9663

  putStrLn "Part 2:"
  putStrLn $ show $ length $ collatzSequence 77671