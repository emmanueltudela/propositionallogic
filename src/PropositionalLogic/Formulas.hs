module PropositionalLogic.Formulas
( Formula(..)
, stringOfFormula
, splittedOnRootOperator
) where

{-# LANGUAGE OverloadedStrings #-}

import PropositionalLogic.Variables
import PropositionalLogic.Operators
import Parenthesis

-- import Debug.Trace (trace)

strip :: String -> String
strip str =
    reverse $ removeStartingSpaces (reverse $ removeStartingSpaces str)
    where removeStartingSpaces "" = ""
          removeStartingSpaces (c:t)
              | c == ' ' = removeStartingSpaces t
              | otherwise = c:t

data Formula = BinaryForm Formula Operator Formula | UnaryForm Operator Formula | Var Variable | Symb SpecialVariable deriving (Show)

parenthesisFormulaLeft :: Operator -> Formula -> String -> String
parenthesisFormulaLeft _ (Symb sv) formulaStr = formulaStr
parenthesisFormulaLeft _ (Var v) formulaStr = formulaStr
parenthesisFormulaLeft _ (UnaryForm op f) formulaStr = formulaStr
parenthesisFormulaLeft opc f@(BinaryForm f1 op f2) formulaStr =
    if op >= opc || (isUnary opc) then
        "(" ++ formulaStr ++ ")"
    else
        formulaStr

parenthesisFormulaRight :: Operator -> Formula -> String -> String
parenthesisFormulaRight _ (Symb sv) formulaStr = formulaStr
parenthesisFormulaRight _ (Var v) formulaStr = formulaStr
parenthesisFormulaRight _ (UnaryForm op f) formulaStr = formulaStr
parenthesisFormulaRight opc f@(BinaryForm f1 op f2) formulaStr =
    if op > opc || (isUnary opc) then
        "(" ++ formulaStr ++ ")"
    else
        formulaStr

stringOfFormula :: Formula -> String
stringOfFormula f =
    case f of Var v -> v
              Symb sv -> stringOfSpecialVariable sv
              UnaryForm op f ->
                  let stringF = parenthesisFormulaRight op f (stringOfFormula f)
                  in (stringOfOperator op) ++ stringF
              BinaryForm f1 op f2 ->
                  let stringF1 = parenthesisFormulaLeft op f1 (stringOfFormula f1)
                      stringF2 = parenthesisFormulaRight op f2 (stringOfFormula f2)
                  in stringF1 ++ (stringOfOperator op) ++ stringF2

rootOperator :: String -> Operator
rootOperator str =
    {- Returns the root operator of the given formula.
     -
     - str must represent a valid Formula
     -}
    let strMp = removeOuterParenthesis (strip str)
        aux acc "" = acc
        aux acc str@(c:t) =
            case (operatorBeginningString str) of
                 Nothing -> aux acc t
                 Just op ->
                     if op < acc then
                         aux op t
                     else
                         aux acc t
    in aux (maxBound :: Operator) strMp

splittedOnRootOperator :: String -> Maybe [String]
splittedOnRootOperator str =
    {- Returns the left and right formula around the root operator
     -
     - str must represent a valid Formula
     -}
    let strMp = removeOuterParenthesis (strip str)
        rootOp = rootOperator str
        aux acc _ "" = Nothing
        aux acc inParenthesis currentStr@(c:t)
            | c == '(' = aux (c:acc) (inParenthesis + 1) t
            | c == ')' = aux (c:acc) (inParenthesis - 1) t
            | otherwise =
                if inParenthesis > 0 then
                    aux (c:acc) inParenthesis t
                else
                    case (operatorBeginningString currentStr) of
                        Just rootOp -> Just [reverse acc, stringWithoutBeginningOperator currentStr]
                        _ -> aux (c:acc) inParenthesis t
    in aux "" 0 str

-- isValidFormulaString :: String -> Bool

formulaOfString :: String -> Formula
formulaOfString str =
    {- Must be a valid string Formula
     -}
    let strMp = removeOuterParenthesis (strip str)
        rootOp = rootOperator strMp
        formulas = splittedOnRootOperator strMp
    in case formulas of
           Nothing ->
               if isSpecialVariableString strMp then
                   Symb (specialVariableOfString strMp)
               else
                   Var strMp
           Just [leftFormStr, rightFormStr] ->
               if isUnary rootOp then
                   UnaryForm rootOp (formulaOfString rightFormStr)
               else
                   BinaryForm (formulaOfString leftFormStr) rootOp (formulaOfString rightFormStr)
