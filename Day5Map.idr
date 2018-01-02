import Data.SortedMap
-- real    14m33.507s
-- user    14m32.018s
-- sys     0m0.463s

inc : Integer -> Integer
inc x = x + 1

jump : Integer -> Integer
jump x =
  if x >= 3
     then x - 1
     else x + 1

record Instruction where
  constructor MkInstruction
  state : SortedMap Nat Integer
  step : Integer
  position : Nat

nextInstruction : Instruction -> Either Integer Instruction
nextInstruction (MkInstruction state step position) =
  case lookup position state of
       Nothing => Left step
       Just value =>
                  let new_state = insert position (inc value) state
                  in
                  Right (MkInstruction new_state (inc step) (cast ((cast position) + value)))

initialInstruction : SortedMap Nat Integer -> Instruction
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

adaptInput : String -> SortedMap Nat Integer
adaptInput file =
  let x = map (cast . trim) $ lines file
  in
  fromList $ zip (natRange (size x)) x

main : IO ()
main = do
  Right file <- readFile "day5.txt"
  let adapted_input = adaptInput file
  putStrLn("Finished:" ++ (show $ findStep $ MkInstruction adapted_input 0 0))
