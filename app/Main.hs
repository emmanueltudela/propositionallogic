module Main where

import PropositionalLogic.Operators
import PropositionalLogic.Variables
import PropositionalLogic.Formulas
import PropositionalLogic.Sequents

import Parenthesis

main :: IO ()
main =
    let f1 = BinaryForm (BinaryForm (Var "A") Impl (Var "B")) Impl (Var "C")
        f2 = BinaryForm (Var "A") Impl (BinaryForm (Var "B") Impl (UnaryForm Not (UnaryForm Not (Var "C"))))
        f3 = BinaryForm (Var "A") Impl (Symb Bot)

        Just [f11, f12] = splittedOnRootOperator "(A->B)->C"
        Just [f21, f22] = splittedOnRootOperator "A->B->~~C"
        Just [f31, f32] = splittedOnRootOperator "A->!"

        s1 = Seq [f1, f2] (Var "D")
    in do putStrLn "f1 :"
          putStrLn (stringOfFormula f1)
          putStrLn ("l : " ++ f11)
          putStrLn ("r : " ++ f12)

          putStrLn ("f2 :")
          putStrLn (stringOfFormula f2)
          putStrLn ("l : " ++ f21)
          putStrLn ("r : " ++ f22)

          putStrLn "f3 :"
          putStrLn (stringOfFormula f3)
          putStrLn ("l : " ++ f31)
          putStrLn ("r : " ++ f32)

          putStrLn "-- Sequent --"
          putStrLn (stringOfSequent s1)
