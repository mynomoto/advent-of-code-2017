%default total

data Direction = N
               | NE
               | SE
               | S
               | SW
               | NW

initialHex : (Int, Int)
initialHex = (0, 0)

-- odd-q (col, row)

isEven : Int -> Bool
isEven x = modNatNZ (cast (abs x)) 2 SIsNotZ == 0

move : (Int, Int) -> Direction -> (Int, Int)
move (x, y) N = (x, y - 1)
move (x, y) NE = if isEven x
                    then (x + 1, y - 1)
                    else (x + 1, y)
move (x, y) SE = if isEven x
                    then (x + 1, y)
                    else (x + 1, y + 1)
move (x, y) S = (x, y + 1)
move (x, y) SW = if isEven x
                    then (x - 1, y)
                    else (x - 1, y + 1)
move (x, y) NW = if isEven x
                    then (x - 1, y - 1)
                    else (x - 1, y)

cubeToOddq : (Int, Int, Int) -> (Int, Int)
cubeToOddq (x, y, z) =
               (case (cast (the Nat) (abs x)) of
                     value => ?result)
               -- let x_adjusted = case (cast the Nat (abs x)) of
               --                       value => ?res
               -- row = z + (cast (divNatNZ (if isEven x
               --               then 0
               --               else 1)(x - (if isEven x
               --               then 0
               --               else 1)) 2 SIsNotZ))
               -- in
               -- (x, row)

-- function oddq_to_cube(hex):
--       x = hex.col
--       z = hex.row - (hex.col - (hex.col&1)) / 2
--       y = -x-z
--       return Cube(x, y, z)
