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

part2 : Vect n String -> Integer

main : IO ()
main = do
  (_ ** lines) <- readVectFile "day2.txt"
  let totalChecksum = part1 lines
  putStrLn $ show totalChecksum
