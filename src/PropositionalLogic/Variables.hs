module PropositionalLogic.Variables
( Variable(..)
, SpecialVariable(..)
, stringOfSpecialVariable
, isSpecialVariableString
, containsSpecialVariable
, specialVariableOfString
) where

{-# LANGUAGE OverloadedStrings #-}

import PropositionalLogic.Operators

import Data.List

data SpecialVariable = Bot deriving (Eq, Show)
type Variable = String

isSpecialVariableString :: String -> Bool
isSpecialVariableString str = str == (stringOfSpecialVariable Bot)

stringOfSpecialVariable :: SpecialVariable -> String
stringOfSpecialVariable sv =
    case sv of Bot -> "False"

beginsWithSpecialVariable :: String -> SpecialVariable -> Bool
beginsWithSpecialVariable str sv =
    let spvStr = stringOfSpecialVariable sv
    in isPrefixOf spvStr str

containsSpecialVariable :: String -> Bool
containsSpecialVariable "" = False
containsSpecialVariable str =
    let (c:t) = str
    in if beginsWithSpecialVariable str Bot then
           True
       else
           containsSpecialVariable t

specialVariableOfString :: String -> Maybe SpecialVariable
specialVariableOfString str
    | str == "False" = Just Bot
    | otherwise = Nothing
