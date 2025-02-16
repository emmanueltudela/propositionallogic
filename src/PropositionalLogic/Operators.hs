module PropositionalLogic.Operators
( Operator(..)
, stringOfOperator
, operatorOfString
, isUnary
, operatorBeginningString
, stringWithoutBeginningOperator
, stringBeginsWithOperator
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

operatorOfString :: String -> Operator
operatorOfString str =
    case str of "~" -> Not
                "/\\" -> And
                "\\/" -> Or
                "->" -> Impl
                "<->" -> Eq

operatorBeginningString :: String -> Maybe Operator
operatorBeginningString str
    | stringBeginsWithOperator str Eq = Just Eq
    | stringBeginsWithOperator str Impl = Just Impl
    | stringBeginsWithOperator str Or = Just Or
    | stringBeginsWithOperator str And = Just And
    | stringBeginsWithOperator str Not = Just Not
    | otherwise = Nothing

stringWithoutBeginningOperator :: String -> String
stringWithoutBeginningOperator str =
    let begOp = operatorBeginningString str
    in case begOp of Nothing -> str
                     Just op -> drop (length (stringOfOperator op)) str

stringBeginsWithOperator :: String -> Operator -> Bool
stringBeginsWithOperator str op =
    let opStr = stringOfOperator op
    in isPrefixOf opStr str
