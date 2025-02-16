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

        Just [f11, f12] = splittedOnMainOperator "(A->B)->C"
        Just [f21, f22] = splittedOnMainOperator "A->B->~~C"
        Just [f31, f32] = splittedOnMainOperator "A->!"

        s1 = Seq [f1, f2] (Var "D")
    in do putStrLn "-- Formulas --"
          putStrLn (stringOfFormula f1)
          putStrLn (stringOfFormula f2)
          putStrLn (stringOfFormula f3)

          putStrLn "-- Main Operator split -- "
          putStrLn "f1 :"
          putStrLn f11
          putStrLn f12

          putStrLn "f2 :"
          putStrLn f21
          putStrLn f22

          putStrLn "f3 :"
          putStrLn f31
          putStrLn f32

          putStrLn "-- Sequent --"
          putStrLn (stringOfSequent s1)
