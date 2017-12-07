import System
import Data.Vect

readLines : File -> IO (n ** Vect n String)
readLines file = do Right line <- fGetLine file | Left err => do putStrLn (show err)
                                                                 pure (_ ** [])
                    eof <- fEOF file
                    if eof
                       then do closeFile file
                               pure (_ ** [])
                       else do (_ ** moreLines) <- readLines file
                               pure (_ ** (line :: moreLines))

readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename =
  do Right file <- openFile filename Read | Left err => do putStrLn (show err)
                                                           pure (_ ** [])
     readLines file

parseLine : String -> List Integer
parseLine x = map cast $
              words x

checksum : List Integer -> Integer
checksum [] = 0
checksum (x :: xs) =
  let min_x = foldl min x xs
      max_x = foldl max x xs
  in
  max_x - min_x

part1 : Vect n String -> Integer
part1 lines = let parsedLines = map parseLine lines
                  linesChecksum = map checksum parsedLines
              in
              sum linesChecksum

dividesEvenly : Integer -> Integer -> Bool
dividesEvenly x y = if mod x y == 0
                       then True
                       else False

checksumEvenly : List Integer -> Integer
checksumEvenly input =
  case input of
       [] => 0
       (x :: xs) =>
            case find (dividesEvenly x) xs of
                 Nothing => checksumEvenly xs
                 (Just y) => div x y

part2 : Vect n String -> Integer
part2 lines =
  let parsedLines = map parseLine lines
      linesChecksum = map (checksumEvenly . reverse . sort) parsedLines
  in
  sum linesChecksum


main : IO ()
main = do
  (_ ** lines) <- readVectFile "day2.txt"
  let totalChecksum = part1 lines
  let evenlyChecksum = part2 lines
  putStrLn $ "Part 1: " ++ show totalChecksum
  putStrLn $ "Part 2: " ++ show evenlyChecksum
