%default total

partial
splitOnHelper : (List (List Char), List Char) -> (match : Char) -> List (List Char)
splitOnHelper (acc, input) match =
  case span (/= match) input of
       (first_word, match :: rest) => splitOnHelper (first_word :: acc, rest) match
       (all, []) => reverse (all :: acc)

partial
splitOn : (input : String) -> (match : Char) -> List String
splitOn input match = map (trim . pack) $ splitOnHelper ([], unpack input) match

startIndex : Nat
startIndex = 0

startSkip : Nat
startSkip = 0

circularTake : List a -> (i : Nat) -> (s : Nat) -> (List a, (List a, List a), List a)
circularTake xs i s =
  let begin = take i xs
      to_take = drop i xs
      taken = take s to_take
      taken_size = size taken
  in
  case compare taken_size s of
       LT => let begin_rest = (drop (cast ((cast s) - (the Integer (cast taken_size)))) begin)
             in
             (begin_rest, (taken, (take (cast ((cast s) - (the Integer (cast taken_size)))) begin)), [])
       EQ => let end = drop s to_take
             in
             (begin, ([], taken), end)
       GT => let end = drop s to_take
             in
             (begin, ([], taken), end)
  -- taken


partial
main : IO ()
main = do
  Right file <- readFile "day10.txt"
  putStrLn $ show $ map (the Nat . cast) $ splitOn file ','
  let list = natRange 256
  putStrLn $ show list
  -- let input = length $ filter not $ map (hasDuplicated . words) $ lines file
  -- let input2 = length $ filter not $ map hasDuplicated $ map (hasAnagram . words) $ lines file
  -- putStrLn $ "Part 1: " ++ show input
  -- putStrLn $ "Part 2: " ++ show input2
