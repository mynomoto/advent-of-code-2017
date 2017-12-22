import Data.SortedMap

record Program where
  constructor MkProgram
  name : String
  weight : Nat
  programs : List String

getInsideParens : List Char -> Maybe (String, String)
getInsideParens ('(' :: xs) = case span (/= ')') xs of
                                   (inside_parens, ')' :: rest) => Just (pack inside_parens, ltrim (pack rest))
                                   _ => Nothing
getInsideParens _ = Nothing

splitOnHelper : (List Char, List Char) -> (match : List Char) -> List Char
splitOnHelper (acc, input) match =
  case input of
       [] => acc
       input => if isPrefixOf match input
                   then ?aaa
                   else ?bbb 

splitOn' : (input : List Char) -> (match : List Char) -> List (List Char)
splitOn' input match =
  if isPrefixOf match input
     then [] --drop (size match) input
     else ?xxx


splitOn : (input : String) -> (match : String) -> List String

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
                                                                      ('-' :: '>' :: ' ' :: names) => MkProgram name (cast weight) [(pack names)]
                                                                      _ =>  MkProgram name (cast weight) ["OI"]

main : IO ()
main = do
  Right file <- readFile "day7-sample.txt"
  let lines = lines file
  putStrLn $ show lines
