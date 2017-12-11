import Data.SortedSet

hasDuplicated : Ord a => List a -> Bool
hasDuplicated xs =
  let s = fromList xs
  in
  length xs /= length (SortedSet.toList s)

main : IO ()
main = do
  Right file <- readFile "day4.txt"
  let input = length $ filter not $ map (hasDuplicated . words) $ lines file
  putStrLn $ show input
