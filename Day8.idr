%default total

data Action = Inc
            | Dec

%name Action act1, act2, act3

Show Action where
  show Inc = "inc"
  show Dec = "dec"

Register : Type
Register = String

Amount : Type
Amount = Integer

data Operator = LT
              | GT
              | EQ
              | LTE
              | GTE
              | NEQ

%name Operator op1, op2, op3

Show Operator where
  show LT = "<"
  show GT = ">"
  show EQ = "=="
  show LTE = "<="
  show GTE = ">="
  show NEQ = "/="

data Condition =
  Cond Operator Register Amount

%name Condition cond1, cond2, cond3

Show Condition where
  show (Cond op1 x y) = "(" ++ (show x) ++ " " ++ (show op1) ++ " " ++ (show y) ++ ")"

data Instruction =
  Inst Register Action Amount Condition

%name Instruction inst1, inst2, inst3

Show Instruction where
  show (Inst x act1 y cond1) = show act1 ++ " " ++ x ++ " by " ++ show y ++ " if " ++ show cond1

parseAction : String -> Maybe Action
parseAction "inc" = Just Inc
parseAction "dec" = Just Dec
parseAction _ = Nothing

parseOperator : String -> Maybe Operator
parseOperator ">" = Just GT
parseOperator "<" = Just LT
parseOperator "==" = Just EQ
parseOperator ">=" = Just GTE
parseOperator "<=" = Just LTE
parseOperator "!=" = Just NEQ
parseOperator _ = Nothing

parseInstruction : String -> Maybe Instruction
parseInstruction input =
  let ws = words input
  in
  case ws of
       (register :: action :: value :: _if :: cond_register :: operator :: cond_value :: []) =>
                   do
                     parsed_action <- parseAction action
                     let parsed_value = the Integer $ cast value
                     let parsed_cond_value = the Integer $ cast cond_value
                     parsed_operator <- parseOperator operator
                     let parsed_condition = Cond parsed_operator cond_register parsed_cond_value
                     Just $ Inst register parsed_action parsed_value parsed_condition
       _ => Nothing


partial
main : IO ()
main = do
  Right file <- readFile "day8-sample.txt"
  let input = lines file
  let parsed_input = sequence $ map parseInstruction input
  putStrLn $ "Part 1: " ++ show parsed_input
  -- putStrLn $ "Part 2: " ++ show input2
