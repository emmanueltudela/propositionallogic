module PropositionalLogic.Sequents
( Sequent(..)
, stringOfSequent
) where

{-# LANGUAGE OverloadedStrings #-}

import PropositionalLogic.Formulas
import Auxiliary

import Data.List.Split

data Sequent = Seq [Formula] Formula deriving (Show)

stringOfFormulas :: [Formula] -> String
stringOfFormulas formulas =
    case formulas of [] -> ""
                     f:[] -> stringOfFormula f
                     f:t -> (stringOfFormula f) ++ ", " ++ (stringOfFormulas t)

stringOfSequent :: Sequent -> String
stringOfSequent (Seq hypothesis proposition) =
    (stringOfFormulas hypothesis) ++ " |- " ++ (stringOfFormula proposition)

areValidFormulas :: [Maybe Formula] -> Bool
areValidFormulas [] = True
areValidFormulas formulas@(formula:t) =
    case formula of
        Nothing -> False
        Just f -> (isValidFormula f) && (areValidFormulas t)

sequentOfString :: String -> Maybe Sequent
sequentOfString "" = Nothing
sequentOfString str =
    let cleanStr = filter (/= ' ') (strip str)
        sequentVars = splitOn "|-" cleanStr
    in if (length sequentVars) /= 2 then
           Nothing
       else
           let [hypothesisStr, propositionStr] = sequentVars
               hypothesis = map (formulaOfString) (filter (/= "") $ splitOn "," hypothesisStr)
               proposition = formulaOfString propositionStr
            in case proposition of
                   Nothing -> Nothing
                   Just p ->
                       if areValidFormulas hypothesis && isValidFormula p then
                           let hs = map (\(Just h) -> h) hypothesis
                           in Just (Seq hs p)
                       else
                           Nothing
