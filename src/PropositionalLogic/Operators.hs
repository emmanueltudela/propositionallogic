module PropositionalLogic.Operators
( Operator(..)
, stringOfOperator
, operatorOfString
, isUnary
, operatorBeginningString
, stringWithoutBeginningOperator
) where

{-# LANGUAGE OverloadedStrings #-}

import Data.List

data Operator = Eq | Impl | Or | And | Not deriving (Ord, Eq, Bounded, Show)

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

operatorOfString :: String -> Maybe Operator
operatorOfString str =
    case str of "~" -> Just Not
                "/\\" -> Just And
                "\\/" -> Just Or
                "->" -> Just Impl
                "<->" -> Just Eq
                _ -> Nothing

operatorBeginningString :: String -> Maybe Operator
operatorBeginningString str
    {- Returns the Operator matching the beginning of the string or
     - Nothing if there is not operator at the beginning.
     -}
    | stringBeginsWithOperator str Eq = Just Eq
    | stringBeginsWithOperator str Impl = Just Impl
    | stringBeginsWithOperator str Or = Just Or
    | stringBeginsWithOperator str And = Just And
    | stringBeginsWithOperator str Not = Just Not
    | otherwise = Nothing

stringWithoutBeginningOperator :: String -> String
stringWithoutBeginningOperator str =
    {- Returns the string without the beginning operator if there is any
     -}
    let begOp = operatorBeginningString str
    in case begOp of Nothing -> str
                     Just op -> drop (length (stringOfOperator op)) str

stringBeginsWithOperator :: String -> Operator -> Bool
stringBeginsWithOperator str op =
    {- Returns wether or not the string begins with the given operator
     -}
    let opStr = stringOfOperator op
    in isPrefixOf opStr str
