-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
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
emptyModel = Model {_cursor_x = 50, _cursor_y = 52, _active = False, _proof = exProof}

exProof :: (Proof Rule Formula)
exProof = SubProof [Formula, Formula] [ProofLine (Line Formula $ RuleSpec Rule [] Formula)] (Line Formula $ RuleSpec Rule [] Formula)

-----------------------------------------------------------------------------