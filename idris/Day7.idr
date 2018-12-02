import Data.SortedMap
import Data.SortedSet

record Program where
  constructor MkProgram
  name : String
  weight : Nat
  programs : List String

record RunningProgram where
  constructor MkRunningProgram
  name : String
  weight : Nat
  programs : List RunningProgram

Show Program where
  show (MkProgram name weight programs) = "Program: " ++ name ++ " (" ++ (show weight) ++ ") " ++ (show programs)

Show RunningProgram where
  show (MkRunningProgram name weight programs) = "RProgram: " ++ name ++ " (" ++ (show weight) ++ ") " ++ (show programs)

getInsideParens : List Char -> Maybe (String, String)
getInsideParens ('(' :: xs) = case span (/= ')') xs of
                                   (inside_parens, ')' :: rest) => Just (pack inside_parens, ltrim (pack rest))
                                   _ => Nothing
getInsideParens _ = Nothing

splitOnHelper : (List (List Char), List Char) -> (match : Char) -> List (List Char)
splitOnHelper (acc, input) match =
  case span (/= match) input of
       (first_word, ',' :: rest) => splitOnHelper (first_word :: acc, rest) match
       (all, []) => reverse (all :: acc)

splitOn : (input : String) -> (match : Char) -> List String
splitOn input match = map (trim . pack) $ splitOnHelper ([], unpack input) match

parseLine : (input : String) -> Program
parseLine input =
  case span (/= ' ') input of
       (name, other_part) =>
                            case getInsideParens $ unpack $ ltrim other_part of
                                 Nothing => MkProgram "" 0 []
                                 (Just (weight, rest)) =>
                                                         if (ltrim rest) == ""
                                                            then MkProgram name (cast weight) []
                                                            else case unpack $ ltrim rest of
                                                                      ('-' :: '>' :: ' ' :: names) => MkProgram name (cast weight) $ splitOn (pack names) ','
                                                                      _ =>  MkProgram name (cast weight) ["OI"]

allProgramNames : List Program -> SortedSet String
allProgramNames xs = foldl (\acc, elem => insert (name elem) acc) empty xs

allProgramNamesAbove : List Program -> SortedSet String
allProgramNamesAbove xs = foldl (\acc, elem => union (fromList (programs elem)) acc) empty xs

bottomProgram : List Program -> List String
bottomProgram xs =
  let all = allProgramNames xs
      all_above = allProgramNamesAbove xs
  in
    SortedSet.toList (difference all all_above)

bottomProgramName : String
bottomProgramName = "qibuqqg"

programMap : List Program -> SortedMap String Program
programMap xs = foldl (\acc , elem => insert (name elem) elem acc) empty xs

findProgram : String -> SortedMap String Program -> Program
findProgram x index = case SortedMap.lookup x index of
                           Nothing => MkProgram "program error" 0 []
                           (Just x) => x

toRunningProgram : Program -> SortedMap String Program -> RunningProgram
toRunningProgram (MkProgram name weight []) index = MkRunningProgram name weight []
toRunningProgram (MkProgram name weight xs) index = MkRunningProgram name weight (map (\s => toRunningProgram (findProgram s index) index) xs)

totalWeight : RunningProgram -> Nat
totalWeight (MkRunningProgram name w []) = w
totalWeight (MkRunningProgram name w rps) = foldl (\acc, rp => acc + (totalWeight rp)) w rps

childrenWeight : RunningProgram -> SortedMap String Nat
childrenWeight (MkRunningProgram n w []) = empty
childrenWeight (MkRunningProgram n w rps) = foldl (\acc, rp => insert (name rp) (totalWeight rp) acc) empty rps

addToGroup : SortedMap Nat (List String) -> (String, Nat) -> SortedMap Nat (List String)
addToGroup acc v@(x, n) =
  case lookup n acc of
       Nothing => insert n [x] acc
       (Just l) => insert n (x :: l) acc

groupBy : List (String, Nat) -> SortedMap Nat (List String)
groupBy [] = empty
groupBy xs = foldl addToGroup empty xs

findDiff : SortedMap Nat (List String) -> (String, Integer)
findDiff summary =
  case toList summary of
       ((v1, (h1 :: t1)) :: (v2, (h2 :: t2)) :: []) =>
                   if size t1 == 0
                      then (h1, (cast v1) - (cast v2))
                      else (h2, (cast v2) - (cast v1))
       _ => ("", -99999)

programWithName : String -> RunningProgram -> Bool
programWithName n (MkRunningProgram name weight programs) = n == name

findTowerDescription : Integer -> SortedMap Nat (List String) -> Nat -> Either Integer (String, Integer)
findTowerDescription x summary weight =
  case toList summary of
       ((v1, (h1 :: t1)) :: []) =>
                   Left (if x > 0
                        then ((cast weight) - x)
                        else ((cast weight) + x))
       ((v1, (h1 :: t1)) :: (v2, (h2 :: t2)) :: []) =>
                   if x > 0 && v1 > v2
                      then Right (h1, (cast v1) - (cast v2))
                      else Right (h2, (cast v2) - (cast v1))
       _ => Left 0

findNextTower : (String, Integer, RunningProgram) -> Either Integer (String, Integer, RunningProgram)
findNextTower (p, n, (MkRunningProgram pname pweight programs)) =
  case find (programWithName p) programs of
       Nothing => Left 0
       Just x =>
                let summary = groupBy $ toList $ childrenWeight x
                in
                case findTowerDescription n summary (weight x) of
                     Right (a, b) => findNextTower (a, b, x)
                     Left l => Left l

main : IO ()
main = do
  Right file <- readFile "day7.txt"
  let lines = lines file
  let programs = map parseLine lines
  let program_map = programMap programs
  let tower = toRunningProgram (findProgram bottomProgramName program_map) program_map
  let summary = groupBy $ toList $ childrenWeight tower
  let diff = findDiff summary
  putStrLn $ show $ findNextTower ((fst diff), (snd diff), tower)
