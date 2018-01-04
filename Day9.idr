%default total

garbage1 : String
garbage1 = "<>"

garbage2 : String
garbage2 = "<random characters>"

garbage3 : String
garbage3 = "<<<<>"

garbage4 : String
garbage4 = "<{!>}>"

garbage5 : String
garbage5 = "<!!>"

garbage6 : String
garbage6 = "<!!!>>"

garbage7 : String
garbage7 = "<{o\"i!a,<{i<a>"

data Garbage = Trash String
%name Garbage garbage

Show Garbage where
  show (Trash x) = "(G: " ++ x ++ ")"

accumulateGarbage : List Char -> List Char -> (Garbage, List Char)
accumulateGarbage acc [] = (Trash (pack acc), [])
accumulateGarbage acc ('>' :: xs) = (Trash (pack (acc ++ ['>'])), [])
accumulateGarbage acc ('!' :: x :: xs) = accumulateGarbage (acc ++ ['!', x]) xs
accumulateGarbage acc (x :: xs) = accumulateGarbage (acc ++ [x]) xs

parseGarbage : List Char -> (Garbage, List Char)
parseGarbage [] = (Trash "", [])
parseGarbage ('<' :: xs) = accumulateGarbage ['<'] xs
parseGarbage xs = (Trash "", xs)

data Group = T Garbage
           | G Group
           | S String
           | Empty

%name Group group

accumulateGroup : Group -> List Char -> (Group, List Char)
accumulateGroup acc ('}' :: xs) = (acc , [])
accumulateGroup xs ys = ?accumulateGroup_rhs

parseGroup : List Char -> (Group, List Char)
parseGroup [] = (S "", [])
parseGroup ('{' :: xs) = accumulateGroup Empty xs
parseGroup xs = (S "", xs)

partial
main : IO ()
main = do
  Right file <- readFile "day9.txt"
  let input = file
  -- let input2 = file
  putStrLn $ "Part 1: " ++ show input
  putStrLn ("garbage 1: " ++ (show $ parseGarbage $ unpack $ garbage1))
  putStrLn ("garbage 2: " ++ (show $ parseGarbage $ unpack $ garbage2))
  putStrLn ("garbage 3: " ++ (show $ parseGarbage $ unpack $ garbage3))
  putStrLn ("garbage 4: " ++ (show $ parseGarbage $ unpack $ garbage4))
  putStrLn ("garbage 5: " ++ (show $ parseGarbage $ unpack $ garbage5))
  putStrLn ("garbage 6: " ++ (show $ parseGarbage $ unpack $ garbage6))
  putStrLn ("garbage 7: " ++ (show $ parseGarbage $ unpack $ garbage7))
  -- putStrLn $ "Part 2: " ++ show input2
