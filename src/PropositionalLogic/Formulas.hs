module PropositionalLogic.Formulas
( Formula(..)
, stringOfFormula
) where

import PropositionalLogic.Variables
import PropositionalLogic.Operators

data Formula = Binary Formula Operator Formula | Unary Operator Formula | Var Variable

parenthesisFormulaLeft :: Operator -> Formula -> String
parenthesisFormulaLeft _ (Var v) = v
parenthesisFormulaLeft opc f@(Binary f1 op f2) =
    if op >= opc then
        "(" ++ (stringOfFormula f) ++ ")"
    else
        stringOfFormula f

parenthesisFormulaRight :: Operator -> Formula -> String
parenthesisFormulaRight _ (Var v) = v
parenthesisFormulaRight _ (Unary op f) = stringOfFormula f
parenthesisFormulaRight opc f@(Binary f1 op f2) =
    if op > opc then
        "(" ++ (stringOfFormula f) ++ ")"
    else
        stringOfFormula f

stringOfFormula :: Formula -> String
stringOfFormula f =
    case f of Var v -> v
              Unary op f ->
                  let stringF = parenthesisFormulaRight op f
                  in (stringOfOperator op) ++ stringF
              Binary f1 op f2 ->
                  let stringF1 = parenthesisFormulaLeft op f1
                      stringF2 = parenthesisFormulaRight op f2
                  in stringF1 ++ (stringOfOperator op) ++ stringF2
