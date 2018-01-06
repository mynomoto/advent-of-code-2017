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

group1 : String
group1 = "{}"

group2 : String
group2 = "{{{}}}"

group3 : String
group3 = "{{},{}}"

group4 : String
group4 = "{{{},{},{{}}}}"

group5 : String
group5 = "{<{},{},{{}}>}"

group6 : String
group6 = "{<a>,<a>,<a>,<a>}"

group7 : String
group7 = "{{<a>},{<a>},{<a>},{<a>}}"

group8 : String
group8 = "{{<!>},{<!>},{<!>},{<a>}}"

group9 : String
group9 = "{{<ab>},{<ab>},{<ab>},{<ab>}}"

group10 : String
group10 = "{{<!!>},{<!!>},{<!!>},{<!!>}}"

group11 : String
group11 = "{{<a!>},{<a!>},{<a!>},{<ab>}}"


data Garbage = Trash String
%name Garbage garbage

Show Garbage where
  show (Trash x) = "(" ++ x ++ ")"

accumulateGarbage : List Char -> List Char -> (Garbage, List Char)
accumulateGarbage acc [] = (Trash (pack acc), [])
accumulateGarbage acc ('>' :: xs) = (Trash (pack (acc ++ ['>'])), xs)
accumulateGarbage acc ('!' :: x :: xs) = accumulateGarbage (acc ++ ['!', x]) xs
accumulateGarbage acc (x :: xs) = accumulateGarbage (acc ++ [x]) xs

parseGarbage : List Char -> (Garbage, List Char)
parseGarbage [] = (Trash "", [])
parseGarbage ('<' :: xs) = accumulateGarbage ['<'] xs
parseGarbage xs = (Trash "", xs)

data String

data Group = T Garbage
           | G (List Group)
           | S String

%name Group group

partial
Show Group where
  show (T garbage) = "T: " ++ show garbage
  show (G xs) = "GRP: {" ++ show xs ++ "}"
  show (S x) = "{" ++ show x ++ "}"

mutual
  partial
  accumulateGroup : Group -> List Char -> (Group, List Char)
  accumulateGroup acc [] = (acc , [])
  accumulateGroup acc ('}' :: xs) = (acc , xs)
  accumulateGroup acc chars@('<' :: _) =
    case parseGarbage chars of
         (trash, rest) =>
                         case acc of
                              G xs => accumulateGroup (G (xs ++ [T trash])) rest
                              single_element => accumulateGroup (G ([single_element] ++ [T trash])) rest
  accumulateGroup acc chars@('{' :: _) =
    case parseGroup chars of
         (group, rest) =>
                         case acc of
                              G xs => accumulateGroup (G (xs ++ [group])) rest
                              single_element => accumulateGroup (G ([single_element] ++ [group])) rest
  accumulateGroup acc chars@(x :: _) =
    case span (\char => char /= '}' && char /= '<' && char /= '{') chars of
         (str, rest) =>
                       case acc of
                            G xs => accumulateGroup (G (xs ++ [S (pack str)])) rest
                            single_element => accumulateGroup (G ([single_element] ++ [S (pack str)])) rest

  partial
  parseGroup : List Char -> (Group, List Char)
  parseGroup [] = (S "", [])
  parseGroup ('{' :: xs) = accumulateGroup (G []) xs
  parseGroup xs = (S "", xs)

partial
score : Int -> Group -> Int
score cs (T garbage) = 0
score cs (G xs) = cs + (foldl (+) 0 (map (score (1 + cs)) xs))
score cs (S x) = 0

partial
main : IO ()
main = do
  Right file <- readFile "day9.txt"
  let input = score 1 $ fst $ parseGroup $ unpack $ file
  putStrLn $ "Part 1: " ++ show input
  -- putStrLn ("garbage 1: " ++ (show $ parseGarbage $ unpack $ garbage1))
  -- putStrLn ("garbage 2: " ++ (show $ parseGarbage $ unpack $ garbage2))
  -- putStrLn ("garbage 3: " ++ (show $ parseGarbage $ unpack $ garbage3))
  -- putStrLn ("garbage 4: " ++ (show $ parseGarbage $ unpack $ garbage4))
  -- putStrLn ("garbage 5: " ++ (show $ parseGarbage $ unpack $ garbage5))
  -- putStrLn ("garbage 6: " ++ (show $ parseGarbage $ unpack $ garbage6))
  -- putStrLn ("garbage 7: " ++ (show $ parseGarbage $ unpack $ garbage7))
  -- putStrLn ("group 1: " ++ (show $ score 1 $ fst $ parseGroup $ unpack $ group1))
  -- putStrLn ("group 2: " ++ (show $ score 1 $ fst $ parseGroup $ unpack $ group2))
  -- putStrLn ("group 3: " ++ (show $ score 1 $ fst $ parseGroup $ unpack $ group3))
  -- putStrLn ("group 4: " ++ (show $ score 1 $ fst $ parseGroup $ unpack $ group4))
  -- putStrLn ("group 5: " ++ (show $ score 1 $ fst $ parseGroup $ unpack $ group5))
  -- putStrLn ("group 6: " ++ (show $ score 1 $ fst $ parseGroup $ unpack $ group6))
  -- putStrLn ("group 7: " ++ (show $ score 1 $ fst $ parseGroup $ unpack $ group7))
  -- putStrLn ("group 8: " ++ (show $ score 1 $ fst $ parseGroup $ unpack $ group8))
  -- putStrLn ("group 9: " ++ (show $ score 1 $ fst $ parseGroup $ unpack $ group9))
  -- putStrLn ("group 10: " ++ (show $ score 1 $ fst $ parseGroup $ unpack $ group10))
  -- putStrLn ("group 11: " ++ (show $ score 1 $ fst $ parseGroup $ unpack $ group11))
  -- let input2 = file
  -- putStrLn $ "Part 2: " ++ show input2
