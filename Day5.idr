import Data.Vect
import Data.Fin

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

inc : Integer -> Integer
inc x = x + 1

record Instruction where
  constructor MkInstruction
  state : Vect n Integer
  step : Integer
  position : Nat

nextInstruction : Instruction -> Either Integer Instruction
nextInstruction (MkInstruction {n} state step position) =
  case natToFin position n of
       Nothing => Left step
       (Just x) =>
                  let value = index x state
                      new_state = updateAt x inc state
                  in
                  Right (MkInstruction new_state (inc step) (position + (cast value)))

main : IO ()
main = do
  (_ ** lines) <- readVectFile "day5.txt"
  let x = map ((+ 0) . cast . trim) lines
  putStrLn $ show x
