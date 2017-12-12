import Data.SortedMap
import Data.SortedSet

hasDuplicated : Ord a => List a -> Bool
hasDuplicated xs =
  let s = fromList xs
  in
  length xs /= length (SortedSet.toList s)

signature : SortedMap Char Nat -> List Char -> SortedMap Char Nat
signature s [] = s
signature s (x :: xs) =
  case lookup x s of
       Nothing => signature (insert x 1 s) xs
       (Just v) => signature (insert x (S v) s) xs

hasAnagram : List String -> List Bool
hasAnagram [] = []
hasAnagram (x :: xs) =
  let signatures = sort $ toList $ signature empty (unpack x)
  in
  case signatures of
       [] => False :: (hasAnagram xs)
       (x :: []) => False :: (hasAnagram xs)
       (x :: y :: xs) =>
            if x == y
               then True :: (hasAnagram (y :: xs))
               else False :: (hasAnagram (y :: xs))

main : IO ()
main = do
  Right file <- readFile "day4.txt"
  let input = length $ filter not $ map (hasDuplicated . words) $ lines file
  let input2 = map (hasAnagram . words) $ lines file
  putStrLn $ show input
  putStrLn $ show $ input2
