module AST where



data Operator
  = Add | Sub | Mul | Div | Vid | Pow
  deriving Show

data Expression
  = Number Int
  | Apply Operator Expression Expression
  deriving Show



eval :: Expression -> Int

eval e = case e of
  Number v -> v
  Apply o x y -> apply o (eval x) (eval y)

  where
    apply o x y = case o of
      Add -> x + y
      Sub -> x - y
      Mul -> x * y
      Div -> x `div` y
      Vid -> y `div` x
      Pow -> x ^ y
