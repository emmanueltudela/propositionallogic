module Main where

import PropositionalLogic.Operators
import PropositionalLogic.Variables
import PropositionalLogic.Formulas
import PropositionalLogic.Sequents

main :: IO ()
main =
    let f1 = BinaryForm (BinaryForm (Var "A") Impl (Var "B")) Impl (Var "C")
        f2 = BinaryForm (Var "A") Impl (BinaryForm (Var "B") Impl (UnaryForm Not (UnaryForm Not (Var "C"))))
        s1 = Seq [f1, f2] (Var "D")
    in do putStrLn (stringOfFormula f1)
          putStrLn (stringOfFormula f2)
          putStrLn (stringOfSequent s1)
