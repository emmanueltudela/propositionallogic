module PropositionalLogic.Formulas
( Formula(..)
, stringOfFormula
, splittedOnRootOperator
) where

{-# LANGUAGE OverloadedStrings #-}

import PropositionalLogic.Variables
import PropositionalLogic.Operators
import Parenthesis
import Auxiliary

-- import Debug.Trace (trace)

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

rootOperator :: String -> Maybe Operator
rootOperator str =
    {- Returns the root operator of the given formula.
     -}
    let strMp = removeOuterParenthesis (strip str)
        aux acc inParenthesis "" = acc
        aux acc inParenthesis str@(c:t) =
            -- Recalculate the parenthesis index
            let newInParenthesis
                    | c == '(' = inParenthesis + 1
                    | c == ')' = inParenthesis - 1
                    | otherwise = inParenthesis
            in case (operatorBeginningString str) of
                 Nothing -> aux acc newInParenthesis t
                 Just op ->
                     case acc of
                         Nothing ->
                             -- Root operator cannot be inside parenthesis
                             if (inParenthesis == 0) then
                                 aux (Just op) newInParenthesis t
                             else
                                 aux acc newInParenthesis t
                         Just opAcc ->
                             -- Only modify root operator if it is of less priority than the previous
                             if (op < opAcc) && (inParenthesis == 0) || (acc == Nothing) then
                                 aux (Just op) newInParenthesis t
                             else
                                 aux acc newInParenthesis t
    in aux Nothing 0 strMp

splittedOnRootOperator :: String -> Maybe [String]
splittedOnRootOperator str =
    {- Returns the left and right formula around the root operator
     -}
    let strMp = removeOuterParenthesis (strip str)
        rootOp = rootOperator str
        aux acc _ "" = Nothing
        aux acc inParenthesis currentStr@(c:t)
            | c == '(' = aux (c:acc) (inParenthesis + 1) t
            | c == ')' = aux (c:acc) (inParenthesis - 1) t
            | otherwise =
                -- Only split on root operator which is outside parenthesis
                if inParenthesis > 0 then
                    aux (c:acc) inParenthesis t
                else
                    -- Only split if the operator is the root operator
                    case (operatorBeginningString currentStr) of
                        Just rootOp -> Just [reverse acc, stringWithoutBeginningOperator currentStr]
                        _ -> aux (c:acc) inParenthesis t
    in aux "" 0 str

isValidFormula :: Formula -> Bool
isValidFormula (Var s) =
    -- A var can't have a special var inside or parenthesis or spaces
    not (' ' `elem` s || '(' `elem` s || ')' `elem` s) && not (s == "") && not (containsSpecialVariable s)
isValidFormula (Symb s) = True
isValidFormula (BinaryForm f1 op f2) = (isValidFormula f1) && (isValidFormula f2)
isValidFormula (UnaryForm op f) = isValidFormula f

isValidFormulaString :: String -> Bool
isValidFormulaString "" = False
isValidFormulaString str =
    let form = formulaOfString str
        validParenthesing = isValidParenthesing str
    in case form of
        Nothing -> False
        Just f -> (isValidFormula f) && validParenthesing

formulaOfString :: String -> Maybe Formula
formulaOfString str =
    let strMp = removeOuterParenthesis (strip str)
        rootOp = rootOperator strMp
        formulas = splittedOnRootOperator strMp
    in case formulas of
           Nothing ->
               -- No Formula so no operator, we are seeing a var or special var
               if isSpecialVariableString strMp then
                   let Just spV = (specialVariableOfString strMp)
                   in Just (Symb spV)
               else
                   Just (Var strMp)
           Just [leftFormStr, rightFormStr] ->
               -- There is some formula that was found so there is a root operator
               let Just op = rootOp
               in if isUnary op then
                      let rightForm = formulaOfString rightFormStr
                      in case rightForm of
                             Nothing -> Nothing
                             Just f -> Just (UnaryForm op f)
                  else
                      let (leftForm, rightForm) = (formulaOfString leftFormStr, formulaOfString rightFormStr)
                      in case (leftForm, rightForm) of
                              (Just f1, Just f2) -> Just (BinaryForm f1 op f2)
                              (_, _) -> Nothing
