import Data.Vect
import Data.Fin
-- real    0m36.631s
-- user    0m36.609s
-- sys     0m0.021s

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

jump : Integer -> Integer
jump x =
  if x >= 3
     then x - 1
     else x + 1

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
                  Right (MkInstruction new_state (inc step) (cast ((cast position) + value)))

initialInstruction : Vect n Integer -> Instruction
initialInstruction state = MkInstruction state 0 0

findStep : Instruction -> Integer
findStep state =
  (case nextInstruction state of
        (Left l) => l
        (Right r) => findStep r)

findStepIO : Instruction -> IO (Either Integer Instruction)
findStepIO instruction@(MkInstruction state step position) = do
  case nextInstruction instruction of
       lv@(Left l) => do
         putStrLn $ show l
         pure lv
       (Right r) => findStepIO r


main : IO ()
main = do
  (_ ** lines) <- readVectFile "day5.txt"
  let x = map ((+ 0) . cast . trim) lines
  putStrLn("Finished:" ++ (show $ findStep $ MkInstruction x 0 0))
