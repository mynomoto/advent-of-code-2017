%default total

data Action = Inc
            | Dec

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

data Condition =
  Cond Operator Register Amount

data Instruction =
  Inst Register Action Amount Condition

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
  let parsed_input = map parseInstruction input
  putStrLn $ "Part 1: " ++ show input
  -- putStrLn $ "Part 2: " ++ show input2
