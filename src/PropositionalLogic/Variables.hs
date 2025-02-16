module PropositionalLogic.Variables
( Variable(..)
, SpecialVariable(..)
, stringOfSpecialVariable
) where

data SpecialVariable = Bot deriving (Eq)
type Variable = String

stringOfSpecialVariable :: SpecialVariable -> String
stringOfSpecialVariable sv =
    case sv of Bot -> "!"
