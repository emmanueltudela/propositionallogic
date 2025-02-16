module Parenthesis
( isValidParenthesing
, removeOuterParenthesis
) where

{-# LANGUAGE OverloadedStrings #-}

isValidParenthesing :: String -> Bool
isValidParenthesing "" = True
isValidParenthesing str =
    let aux cmpt "" = cmpt == 0
        aux cmpt (c:t)
            | c == '(' = aux (cmpt + 1) t
            | c == ')' = aux (cmpt - 1) t
            | otherwise = aux cmpt t
    in aux 0 str

isolateToExcessClosingParenthesis :: String -> String
isolateToExcessClosingParenthesis "" = ""
isolateToExcessClosingParenthesis str =
    {- Returns the string up to the first excess closing parenthesis
     - (Up to the first closing parenthesis that doesn't match with
     - any opening parenthesis)
     -}
    let aux cmpt acc "" = acc
        aux cmpt acc (c:t) =
            case c of '(' -> aux (cmpt + 1) (c:acc) t
                      ')' -> if cmpt - 1 < 0 then
                                acc
                             else
                                aux (cmpt - 1) (c:acc) t
                      char -> aux cmpt (c:acc) t
    in reverse (aux 0 "" str)

removeOuterParenthesis :: String -> String
removeOuterParenthesis "" = ""
removeOuterParenthesis (c:t)
    {- Remove the first parenthesis then isolate the string to the first
     - excess closing parenthesis. If the string only lose 1 char then
     - the excess parenthesis was at the end of the string. It implies
     - that we removed outer parenthesis. If the string lose more than 1
     - char then the parenthesis wasn't surrounding the whole expression
     - and there was no surrounding parenthesing.
     -
     - Parenthesing must be valid.
     -}
    | c /= '(' = c:t
    | otherwise =
        let remainingValidParenthesing = isolateToExcessClosingParenthesis t
        in if (length remainingValidParenthesing) == (length t) - 1 then
               removeOuterParenthesis remainingValidParenthesing
           else
               c:t

