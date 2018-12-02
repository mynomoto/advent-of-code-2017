import Data.SortedSet

%default total

hasDuplicated : Ord a => List a -> Bool
hasDuplicated xs =
  let s = fromList xs
  in
  length xs /= length (SortedSet.toList s)

hasAnagram : List String -> List (List Char)
hasAnagram [] = []
hasAnagram (x :: xs) =
  let signatures = (sort . unpack) x
  in
  signatures :: hasAnagram xs

partial
main : IO ()
main = do
  Right file <- readFile "day4.txt"
  let input = length $ filter not $ map (hasDuplicated . words) $ lines file
  let input2 = length $ filter not $ map hasDuplicated $ map (hasAnagram . words) $ lines file
  putStrLn $ "Part 1: " ++ show input
  putStrLn $ "Part 2: " ++ show input2
