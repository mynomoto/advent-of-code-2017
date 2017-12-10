import Data.SortedMap

Position : Type
Position = (Integer, Integer)

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

startPosition : Position
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
  position : Position

startMemory : Memory
startMemory = MkMemory startDirection startNumber startStep startPosition

nextPosition : Position -> Direction -> Integer -> Position
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
streamMemory = iterate nextStep startMemory

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

part1 : Integer -> Memory -> Integer
part1 input (MkMemory direction n step position) =
  let missing_steps = input - n
      (a, b) = nextPosition position direction missing_steps
  in
  abs(a) + abs(b)

InMemory : Type
InMemory = SortedMap Position Integer

move : Position -> Direction -> InMemory -> (Position, Direction)
move position direction memory =
  let new_direction = nextDirection direction
      new_position = nextPosition position new_direction 1
  in
  case lookup new_position memory of
       Nothing => (new_position, new_direction)
       (Just x) => (nextPosition position direction 1, direction)

record Memory2 where
  constructor MkMemory2
  direction : Direction
  position : Position
  memory : InMemory
  value : Integer

findNeighbours : Position -> List Position
findNeighbours (x, y) =
  [ (x + 1, y)
  , (x - 1, y)
  , (x + 1, y + 1)
  , (x - 1, y + 1)
  , (x + 1, y - 1)
  , (x - 1, y - 1)
  , (x, y + 1)
  , (x, y - 1)
  ]

sumJust : Integer -> Maybe Integer -> Integer
sumJust x Nothing = x
sumJust x (Just y) = x + y

neighboursSum : InMemory -> Position -> Integer
neighboursSum in_memory position =
  let neighbours = findNeighbours position
  in
  foldl sumJust 0 $ map (\x => lookup x in_memory) neighbours

nextMemory : Memory2 -> Memory2
nextMemory (MkMemory2 direction position memory value) =
  let (next_position, next_direction) = move position direction memory
      new_value = neighboursSum memory next_position
      new_memory = insert next_position new_value memory
  in
  MkMemory2 next_direction next_position new_memory new_value

startPosition2 : Position
startPosition2 = (1, 0)

startMemory2 : Memory2
startMemory2 = MkMemory2 startDirection startPosition2 (insert startPosition2 startNumber (insert startPosition startNumber empty)) 1

part2 : Integer -> Memory2 -> Integer
part2 input memory2@(MkMemory2 direction position memory value) =
  if value > input
     then value
     else part2 input (nextMemory memory2)

main : IO ()
main = do
  putStrLn $ show $ part1 input $ closerN input startMemory streamMemory
  putStrLn $ show $ part2 input startMemory2
