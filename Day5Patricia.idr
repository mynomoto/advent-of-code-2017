import Patricia.IntMap
-- real    11m12.423s
-- user    11m12.047s
-- sys     0m0.110s

inc : Integer -> Integer
inc x = x + 1

jump : Integer -> Integer
jump x =
  if x >= 3
     then x - 1
     else x + 1

record Instruction where
  constructor MkInstruction
  state : IntBitMap 32 Integer
  step : Integer
  position : Integer

nextInstruction : Instruction -> Either Integer Instruction
nextInstruction (MkInstruction state step position) =
  case lookup position state of
       Nothing => Left step
       Just value =>
                  let new_state = insert position (inc value) state
                  in
                  Right (MkInstruction new_state (inc step) (position + value))

initialInstruction : IntBitMap 32 Integer -> Instruction
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

adaptInput : String -> IntBitMap 32 Integer
adaptInput file =
  let x = map (cast . trim) $ lines file
  in
  fromList $ zip (map cast (natRange (size x))) x

main : IO ()
main = do
  Right file <- readFile "day5.txt"
  let adapted_input = adaptInput file
  putStrLn("Finished:" ++ (show $ findStep $ MkInstruction adapted_input 0 0))
