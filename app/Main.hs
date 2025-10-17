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
emptyModel :: (Model Formula Rule)
emptyModel = Model {_cursorX = 50, _cursorY = 52, _active = False, _proof = exProof}

exProof :: (Proof Formula Rule)
exProof = SubProof [Formula, Formula] [ProofLine $ Derivation Formula Rule []] (Derivation Formula Rule [])

-----------------------------------------------------------------------------