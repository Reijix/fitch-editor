module Main where

import App
import Syntax

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
main :: IO ()
main = runApp emptyModel

-----------------------------------------------------------------------------
data Rule = Rule deriving (Show, Eq)

data Formula = Formula deriving (Show, Eq)

-----------------------------------------------------------------------------
emptyModel :: (Model Rule Formula)
emptyModel = Model {_cursorX = 50, _cursorY = 52, _active = False, _proof = exProof}

exProof :: (Proof Rule Formula)
exProof = SubProof [Formula, Formula] [ProofLine (Line Formula $ RuleSpec Rule [] Formula)] (Line Formula $ RuleSpec Rule [] Formula)

-----------------------------------------------------------------------------