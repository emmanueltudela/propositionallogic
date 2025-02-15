module PropositionalLogic.Formulas
( Formula(..)
, stringOfFormula
) where

import PropositionalLogic.Variables
import PropositionalLogic.Operators

-- import Debug.Trace (trace)

data Formula = BinaryForm Formula Operator Formula | UnaryForm Operator Formula | Var Variable

parenthesisFormulaLeft :: Operator -> Formula -> String -> String
parenthesisFormulaLeft _ (Var v) formulaStr = formulaStr
parenthesisFormulaLeft _ (UnaryForm op f) formulaStr = formulaStr
parenthesisFormulaLeft opc f@(BinaryForm f1 op f2) formulaStr =
    if op >= opc || (isUnary opc) then
        "(" ++ formulaStr ++ ")"
    else
        formulaStr

parenthesisFormulaRight :: Operator -> Formula -> String -> String
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
              UnaryForm op f ->
                  let stringF = parenthesisFormulaRight op f (stringOfFormula f)
                  in (stringOfOperator op) ++ stringF
              BinaryForm f1 op f2 ->
                  let stringF1 = parenthesisFormulaLeft op f1 (stringOfFormula f1)
                      stringF2 = parenthesisFormulaRight op f2 (stringOfFormula f2)
                  in stringF1 ++ (stringOfOperator op) ++ stringF2
