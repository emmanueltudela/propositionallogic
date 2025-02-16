module PropositionalLogic.Formulas
( Formula(..)
, stringOfFormula
, splittedOnMainOperator
) where

{-# LANGUAGE OverloadedStrings #-}

import PropositionalLogic.Variables
import PropositionalLogic.Operators
import Parenthesis

-- import Debug.Trace (trace)

data Formula = BinaryForm Formula Operator Formula | UnaryForm Operator Formula | Var Variable | Symb SpecialVariable

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

minimumMainOperator :: String -> Operator
minimumMainOperator str =
    let aux acc "" = acc
        aux acc str@(c:t) =
            case (operatorBeginningString str) of
                 Nothing -> aux acc t
                 Just op ->
                     if op < acc then
                         aux op t
                     else
                         aux acc t
    in aux (maxBound :: Operator) str

splittedOnMainOperator :: String -> Maybe [String]
splittedOnMainOperator str =
    let strMp = removeOuterParenthesis str
        minOp = minimumMainOperator str
        aux acc _ "" = Nothing
        aux acc inParenthesis currentStr@(c:t)
            | c == '(' = aux (c:acc) (inParenthesis + 1) t
            | c == ')' = aux (c:acc) (inParenthesis - 1) t
            | otherwise =
                if inParenthesis > 0 then
                    aux (c:acc) inParenthesis t
                else
                    if stringBeginsWithOperator currentStr minOp then
                        Just [reverse acc, stringWithoutBeginningOperator currentStr]
                    else
                        aux (c:acc) inParenthesis t
    in aux "" 0 str

-- formulaOfString :: String -> Maybe Formula
-- formulaOfString "" = Nothing
-- formulaOfString str = Nothing
--     let aux _ _ "" = Nothing
--         aux form currentOp str =
--             case str of '(':t ->
