import Data.Nat.Views
import Data.SortedMap
%default total

Position : Type
Position = (Integer, Integer)

input : Nat
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

stepSize : Nat -> Nat
stepSize x = divNatNZ x 2 SIsNotZ

stepSize2 : Nat -> Nat
stepSize2 k with (half k)
  stepSize2 (S (n + n)) | HalfOdd = n
  stepSize2 (n + n) | HalfEven = n

realStep : Nat -> Nat
realStep x = x + 1

startStep : Nat
startStep = 1

startPosition : Position
startPosition = (0, 0)


startDirection : Direction
startDirection = Right

startNumber : Nat
startNumber = 1

record Memory where
  constructor MkMemory
  direction : Direction
  n : Nat
  step : Nat
  position : Position

startMemory : Memory
startMemory = MkMemory startDirection startNumber startStep startPosition

nextPosition : Position -> Direction -> Nat -> Position
nextPosition (x, y) Right step_size = (x + (cast step_size), y)
nextPosition (x, y) Up step_size = (x, y + (cast step_size))
nextPosition (x, y) Left step_size = (x - (cast step_size), y)
nextPosition (x, y) Down step_size = (x, y - (cast step_size))

nextStep : Memory -> Memory
nextStep (MkMemory direction n step position) =
  let rs = realStep step
      step_size = stepSize2 rs
  in
  MkMemory (nextDirection direction) (n + step_size) (step + 1) (nextPosition position direction step_size)

streamMemory : Stream Memory
streamMemory = iterate nextStep startMemory

partial
closerN : Nat -> Memory -> Stream Memory -> Memory
closerN target initialMemory (currentMemory@(MkMemory direction n step position) :: xs) =
  case compare target n of
       LT => initialMemory
       EQ => currentMemory
       GT => closerN target currentMemory xs

displayMemory : Memory -> String
displayMemory (MkMemory direction n step position) = "Direction: " ++
                                                     (displayDirection direction) ++
                                                     ", n: " ++
                                                     (show n) ++
                                                     ", step: " ++
                                                     (show step) ++
                                                     ", position: " ++
                                                     (show position)

part1 : Nat -> Memory -> Integer
part1 input (MkMemory direction n step position) =
  let missing_steps = minus input n
      (a, b) = nextPosition position direction missing_steps
  in
  abs(a) + abs(b)

InMemory : Type
InMemory = SortedMap Position Nat

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
  value : Nat

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

sumJust : Nat -> Maybe Nat -> Nat
sumJust x Nothing = x
sumJust x (Just y) = x + y

neighboursSum : InMemory -> Position -> Nat
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

partial
part2 : Nat -> Memory2 -> Nat
part2 input memory2@(MkMemory2 direction position memory value) =
  if value > input
     then value
     else part2 input (nextMemory memory2)

partial
main : IO ()
main = do
  putStrLn $ show $ part1 input $ closerN input startMemory streamMemory
  putStrLn $ show $ part2 input startMemory2
