
input : Integer
input = 277678

data Direction = Right
               | Up
               | Left
               | Down

displayDirection : Direction -> String
displayDirection Right = "Right"
displayDirection Up = "Up"
displayDirection Left = "Left"
displayDirection Down = "Down"

nextDirection : Direction -> Direction
nextDirection Right = Up
nextDirection Up = Left
nextDirection Left = Down
nextDirection Down = Right

stepSize : Integer -> Integer
stepSize x = div x 2

realStep : Integer -> Integer
realStep x = x + 1

startStep : Integer
startStep = 1

startPosition : (Integer, Integer)
startPosition = (0, 0)

startDirection : Direction
startDirection = Right

startNumber : Integer
startNumber = 1

record Memory where
  constructor MkMemory
  direction : Direction
  n : Integer
  step : Integer
  position : (Integer, Integer)

startWorld : Memory
startWorld = MkMemory startDirection startNumber startStep startPosition

nextPosition : (Integer, Integer) -> Direction -> Integer -> (Integer, Integer)
nextPosition (x, y) Right step_size = (x + step_size, y)
nextPosition (x, y) Up step_size = (x, y + step_size)
nextPosition (x, y) Left step_size = (x - step_size, y)
nextPosition (x, y) Down step_size = (x, y - step_size)

nextStep : Memory -> Memory
nextStep (MkMemory direction n step position) =
  let rs = realStep step
      step_size = stepSize rs
  in
  MkMemory (nextDirection direction) (n + step_size) (step + 1) (nextPosition position direction step_size)

streamMemory : Stream Memory
streamMemory = iterate nextStep startWorld

closerN : Integer -> Memory -> Stream Memory -> Memory
closerN target initialMemory (currentMemory@(MkMemory direction n step position) :: xs) =
  (case compare target n of
        LT => initialMemory
        EQ => currentMemory
        GT => closerN target currentMemory xs)

displayMemory : Memory -> String
displayMemory (MkMemory direction n step position) = "Direction: " ++
                                                     (displayDirection direction) ++
                                                     ", n: " ++
                                                     (show n) ++
                                                     ", step: " ++
                                                     (show step) ++
                                                     ", position: " ++
                                                     (show position)

main : IO ()
main = putStrLn $ displayMemory $ closerN 1024 startWorld streamMemory

  -- if n > target
  --    then initialMemory
  --    else if n == target
  --         then currentMemory
  --         else closerN target currentMemory xs
