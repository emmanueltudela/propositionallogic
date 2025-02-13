module Main where

import PropositionalLogic.Operators
import PropositionalLogic.Variables
import PropositionalLogic.Formulas

main :: IO ()
main =
    let f1 = Form (Form (Var "A") Impl (Var "B")) Impl (Var "C")
        f2 = Form (Var "A") Impl (Form (Var "B") And (Var "C"))
    in do putStrLn (stringOfFormula f2)
