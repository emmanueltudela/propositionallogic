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

isolateToExcessParenthesis :: String -> String
isolateToExcessParenthesis "" = ""
isolateToExcessParenthesis str =
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
    | c /= '(' = c:t
    | otherwise =
        let remainingValidParenthesing = isolateToExcessParenthesis t
        in if (length remainingValidParenthesing) == (length t) - 1 then
               removeOuterParenthesis remainingValidParenthesing
           else
               c:t

