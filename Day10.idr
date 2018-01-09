import Data.Bits

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

circularTake : List a -> (i : Nat) -> (s : Int) -> (List a, (List a, List a), List a)
circularTake xs i s =
  let begin = take i xs
      to_take = drop i xs
      taken = take (cast s) to_take
      taken_size = size taken
  in
  case compare taken_size (cast s) of
       LT => let begin_rest = (drop (cast ((cast s) - (the Integer (cast taken_size)))) begin)
             in
             (begin_rest, (taken, (take (cast ((cast s) - (the Integer (cast taken_size)))) begin)), [])
       EQ => let end = drop (cast s) to_take
             in
             (begin, (taken, []), end)
       GT => let end = drop (cast s) to_take
             in
             (begin, (taken, []), end)

reverseCircularTaken : (List a, (List a, List a), List a) -> List a
reverseCircularTaken (begin, (taken_from_end, taken_from_begin), end) =
  let taken_from_end_size = length taken_from_end
      taken_from_begin_size = length taken_from_begin
      reverse_taken = reverse(taken_from_end ++ taken_from_begin)
  in
  (drop taken_from_end_size reverse_taken) ++ begin ++ (take taken_from_end_size reverse_taken) ++ end

partial
calculateHash : (idx : Nat) -> (skip : Nat) -> (sizes : List Int) -> (list : List a) -> List a
calculateHash idx skip [] list = list
calculateHash idx skip (x :: xs) list =
  let new_list = reverseCircularTaken $ circularTake list idx x
  in
  calculateHash (modNat (idx + (cast x) + skip) (length list)) (S skip) xs new_list

standardSuffix : List Int
standardSuffix = [17, 31, 73, 47, 23]

replicateList : Nat -> List a -> List a
replicateList Z xs = xs
replicateList (S k) xs = xs ++ (replicateList k xs)

partial
denseHash : List (Bits 8) -> List Nat -> List (Bits 8)
denseHash acc [] = acc
denseHash acc x =
  let to_hash = take 16 x
      hash = foldl xor (intToBits 0) $ map (intToBits . cast) to_hash
  in
  denseHash (acc ++ [hash]) (drop 16 x)

hexStringValues : List String
hexStringValues = map singleton (unpack "0123456789abcdef")

toHexC : Nat -> String
toHexC n =
  case inBounds n hexStringValues of
       (Yes prf) => index n hexStringValues
       (No contra) => "x"

zeroF : Bits 8
zeroF = intToBits 0x0f

toHexI : Int -> String
toHexI n =
  let l = n `shiftR` 4
      r = (bitsToInt (and (intToBits (cast n)) zeroF))
  in
  (toHexC (cast l)) ++ (toHexC (cast r))

toHex : List Int -> String
toHex = concatMap toHexI

partial
main : IO ()
main = do
  Right file <- readFile "day10.txt"
  let input = map (the Int . cast) $ splitOn file ','
  let list = natRange 256
  let sample_list = natRange 5
  let sample_input = [3, 4, 1, 5]
  let final_list = calculateHash startIndex startSkip input list
  case final_list of
       x :: y :: xs => putStrLn $ "Part 1: " ++ show (x * y)
       x => putStrLn $ "Part 1 (ERROR): " ++ show x

  let sample_file = "1,2,3"
  let input2 = (map ord$ unpack sample_file) ++ standardSuffix
  let input2_64rounds = replicateList 63 input2
  let hash_2 = calculateHash startIndex startSkip input2_64rounds list
  let hex_hash = toHex $ map (fromInteger . bitsToInt) $ denseHash [] hash_2
  putStrLn hex_hash

  -- putStrLn $ show $ toHex $ map (fromInteger . bitsToInt) $ denseHash $ map intToBits hash_2
  -- putStrLn $ "Part 2: " ++ show input2
