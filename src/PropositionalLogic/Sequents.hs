module PropositionalLogic.Sequents
( Sequent(..)
, stringOfSequent
) where

import PropositionalLogic.Formulas

data Sequent = Seq [Formula] Formula

stringOfFormulas :: [Formula] -> String
stringOfFormulas formulas =
    case formulas of [] -> ""
                     f:[] -> stringOfFormula f
                     f:t -> (stringOfFormula f) ++ ", " ++ (stringOfFormulas t)

stringOfSequent :: Sequent -> String
stringOfSequent (Seq hypothesis proposition) =
    (stringOfFormulas hypothesis) ++ " |- " ++ (stringOfFormula proposition)
