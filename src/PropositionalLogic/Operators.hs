module PropositionalLogic.Operators
( Operator(..)
, stringOfOperator
, operatorOfString
, isUnary
) where

data Operator = Eq | Impl | Or | And | Not deriving (Ord, Eq)

isUnary :: Operator -> Bool
isUnary op =
    case op of Not -> True
               _ -> False

stringOfOperator :: Operator -> String
stringOfOperator op =
    case op of Not -> "~"
               And -> "/\\"
               Or -> "\\/"
               Impl -> "->"
               Eq -> "<->"

operatorOfString :: String -> Operator
operatorOfString str =
    case str of "~" -> Not
                "/\\" -> And
                "\\/" -> Or
                "->" -> Impl
                "<->" -> Eq
