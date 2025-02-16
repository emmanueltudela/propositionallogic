module PropositionalLogic.Variables
( Variable(..)
, SpecialVariable(..)
, stringOfSpecialVariable
, isSpecialVariableString
, specialVariableOfString
) where

data SpecialVariable = Bot deriving (Eq, Show)
type Variable = String

isSpecialVariableString :: String -> Bool
isSpecialVariableString str = str == (stringOfSpecialVariable Bot)

stringOfSpecialVariable :: SpecialVariable -> String
stringOfSpecialVariable sv =
    case sv of Bot -> "!"

specialVariableOfString :: String -> SpecialVariable
specialVariableOfString str
    | str == "!" = Bot
