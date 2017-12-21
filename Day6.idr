import Data.SortedSet
import Data.SortedMap

Memory : Type
Memory = SortedMap Nat Nat

listToMap : List Nat -> Memory
listToMap xs = fromList $ zip (natRange (size xs)) xs

wrapIndex : Nat -> Nat
wrapIndex k = mod k 16

wrapIndexs : Nat -> Nat
wrapIndexs k = mod k 4

isMaxKey : Nat -> (Nat, Nat) -> Bool
isMaxKey k (a, b) = k == b

resetBank : Nat -> Memory -> Memory
resetBank k x = insert k 0 x

relocate : (idx : Nat) -> (value : Nat) -> Memory -> Memory
relocate idx Z x = x
relocate idx (S k) x =
  case lookup idx x of
       Nothing => x
       (Just c) => relocate (wrapIndex (S idx)) k (insert idx (S c) x)

updateMemory : Memory -> Memory
updateMemory x =
  let l = toList x
      ms = foldl1 max $ values x
   in
   case find (isMaxKey ms) l of
        Nothing => x
        (Just p) => relocate (wrapIndex (S (fst p))) (snd p) (resetBank (fst p) x)

allMemories : (input : Memory) -> Stream Memory
allMemories input = iterate updateMemory input

findStep : Stream Memory -> Nat -> SortedSet (List (Nat, Nat)) -> Nat
findStep (value :: xs) k s =
  case contains (toList value) s of
       False => findStep xs (S k) (insert (toList value) s)
       True => k

findCycleStep : Stream Memory -> Nat -> SortedMap (List (Nat, Nat)) Nat -> Integer
findCycleStep (value :: xs) k s =
  case lookup (toList value) s of
       Nothing => findCycleStep xs (S k) (insert (toList value) (S k) s)
       (Just x) => (cast (S k)) - (cast x)

main : IO ()
main = do
  Right file <- readFile "day6.txt"
  let values = map cast $ words file
  let m = listToMap values
  let s = listToMap [0, 2, 7, 0]
  putStrLn $ show $ findCycleStep (allMemories s) 0 empty
