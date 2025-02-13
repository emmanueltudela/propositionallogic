module PropositionalLogic.Operators
( Operator(..)
, stringOfOperator
) where

data Operator = Impl deriving (Enum)

stringOfOperator :: Operator -> String
stringOfOperator op = case op of Impl -> "->"
