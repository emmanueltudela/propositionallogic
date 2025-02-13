module PropositionalLogic.Operators
( Operator(..)
, stringOfOperator
, operatorOfString
) where

data Operator = Eq | Impl | Or | And | Not deriving (Ord, Eq)

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
