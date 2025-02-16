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

rootOperator :: String -> Maybe Operator
rootOperator str =
    {- Returns the root operator of the given formula.
     -}
    let strMp = removeOuterParenthesis (strip str)
        aux acc inParenthesis "" = acc
        aux acc inParenthesis str@(c:t) =
            let newInParenthesis
                    | c == '(' = inParenthesis + 1
                    | c == ')' = inParenthesis - 1
                    | otherwise = inParenthesis
            in case (operatorBeginningString str) of
                 Nothing -> aux acc newInParenthesis t
                 Just op ->
                     case acc of
                         Nothing ->
                             if (inParenthesis == 0) then
                                 aux (Just op) newInParenthesis t
                             else
                                 aux acc newInParenthesis t
                         Just opAcc ->
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
                if inParenthesis > 0 then
                    aux (c:acc) inParenthesis t
                else
                    case (operatorBeginningString currentStr) of
                        Just rootOp -> Just [reverse acc, stringWithoutBeginningOperator currentStr]
                        _ -> aux (c:acc) inParenthesis t
    in aux "" 0 str

isValidFormulaString :: String -> Bool
isValidFormulaString "" = False
isValidFormulaString str =
    let form = formulaOfString str
        validParenthesing = isValidParenthesing str
        aux (Var s) = not (' ' `elem` s || '(' `elem` s || ')' `elem` s) && not (s == "") && not (containsSpecialVariable s)
        aux (Symb s) = True
        aux (BinaryForm f1 op f2) = (aux f1) && (aux f2)
        aux (UnaryForm op f) = aux f
    in case form of
        Nothing -> False
        Just f -> (aux f) && validParenthesing

formulaOfString :: String -> Maybe Formula
formulaOfString str =
    let strMp = removeOuterParenthesis (strip str)
        rootOp = rootOperator strMp
        formulas = splittedOnRootOperator strMp
    in case formulas of
           Nothing ->
               if isSpecialVariableString strMp then
                   let Just spV = (specialVariableOfString strMp)
                   in Just (Symb spV)
               else
                   Just (Var strMp)
           Just [leftFormStr, rightFormStr] ->
               case rootOp of
                   Nothing -> Nothing
                   Just op ->
                       if isUnary op then
                           let rightForm = formulaOfString rightFormStr
                           in case rightForm of
                                  Nothing -> Nothing
                                  Just f -> Just (UnaryForm op f)
                       else
                           let (leftForm, rightForm) = (formulaOfString leftFormStr, formulaOfString rightFormStr)
                           in case (leftForm, rightForm) of
                                   (Just f1, Just f2) -> Just (BinaryForm f1 op f2)
                                   (_, _) -> Nothing
