module PropositionalLogic.Variables
( Variable(..)
, SpecialVariable
) where

data SpecialVariable = Bot deriving (Eq)
type Variable = String

stringOfSpecialVariable :: SpecialVariable -> string
stringOfSpecialVariable sv =
    case sv of Bot -> "!"
