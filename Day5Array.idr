import Data.Array

inc : Integer -> Integer
inc x = x + 1

jump : Integer -> Integer
jump x =
  if x >= 3
     then x - 1
     else x + 1

record Instruction where
  constructor MkInstruction
  state : IOArray Integer
  step : Integer
  position : Int

Show Instruction where
  show (MkInstruction state step position) = "Instruction: " ++ (show step) ++ " (" ++ (show position) ++ ")"

nextInstruction : Instruction -> IO(Either Integer Instruction)
nextInstruction (MkInstruction state step position) =
  do
    True <- pure $ position < length state | pure (Left step)
    value <- index position state
    setAt position (inc value) state
    pure $ Right (MkInstruction state (inc step) (if (fromInteger value) >= 0
                                                     then (position + (fromInteger value))
                                                     else (position - (abs (fromInteger value)))))

findStep : Instruction -> IO(Integer)
findStep state =
  do
    case !(nextInstruction state) of
         Left l => pure l
         Right r => findStep r


adaptInput : String -> IO(IOArray Integer)
adaptInput file =
  let x = map (cast . trim) $ lines file
  in
  fromList $ x

main : IO ()
main = do
  Right file <- readFile "day5.txt"
  array <- adaptInput file
  let initial_instruction = MkInstruction array 0 0
  putStrLn $ show $ length array
  step <- findStep initial_instruction
  putStrLn $ "Finished:" ++ (show step)

-- import Data.Array

-- main : IO ()
-- main = do
--     (cmd::args) <- getArgs
--     arr <- fromList $ map unpack args
--     printLn (length arr)
--     main' arr
--   where main' : IOArray (List Char) -> IO ()
--         main' arr = do
--           putStr "> "
--           line <- getLine
--           Just ix <- pure $ parsePositive {a = Int} line | pure ()
--           True <- pure $ ix < length arr | pure ()
--           printLn !(index ix arr)
-- main' arr
