module Syntax
  ( RuleSpec (RuleSpec),
    Line (Line),
    Proof (PhantomProof, ProofLine, SubProof),
    pLength,
    pLookup,
    Model (..),
    Action (PointerDown, PointerUp, HandlePointer),
    active,
    cursorX,
    cursorY,
    proof,
  )
where

import qualified Data.List as L
import Miso
  ( App,
    Attribute,
    Component (events, subs),
    Effect,
    MisoString,
    PointerEvent (client),
    ROOT,
    View,
    component,
    consoleLog,
    io_,
    mouseSub,
    ms,
    onWithOptions,
    pointerDecoder,
    preventDefault,
    run,
    startApp,
    text,
  )
import qualified Miso.CSS as CSS
import Miso.Lens (Lens, lens, this, use, (.=), (^.))
import Miso.Svg (text_, tspan_)
import qualified Miso.Svg.Element as S
import qualified Miso.Svg.Property as SP

data RuleSpec rule formula = RuleSpec rule [Either formula (Proof rule formula)] formula deriving (Show, Eq)

data Line rule formula = Line formula (RuleSpec rule formula) deriving (Show, Eq)

-- TODO add phantom proof/line, idea: Phantom Int (Int gives number of lines that are phantomed)
data Proof rule formula where
  PhantomProof :: Int -> Proof rule formula
  ProofLine :: Line rule formula -> Proof rule formula
  SubProof :: [formula] -> [Proof rule formula] -> Line rule formula -> Proof rule formula
  deriving (Show, Eq)

pLength :: Proof rule formula -> Int
pLength (PhantomProof n) = n
pLength (ProofLine l) = 1
pLength (SubProof fs ps _) = foldr (\p n -> pLength p + n) (L.length fs + 1) ps

pLookup :: Proof rule formula -> Int -> Either (Line rule formula) formula
pLookup (PhantomProof _) _ = error "Tried (!!) on PhantomProof"
pLookup (ProofLine l) 0 = Left l
pLookup (ProofLine _) _ = error "Tried (!!) on ProofLine with n > 0"
pLookup (SubProof fs _ _) n | n < L.length fs = Right $ fs L.!! n
pLookup (SubProof _ [] l) 0 = Left l
pLookup (SubProof _ [] _) _ = error "Tried (!!) on SubProof fs [] l with n > 0"
pLookup (SubProof fs (p : _) _) n | n < pLength p = pLookup p n
pLookup (SubProof fs (_ : ps) l) n = pLookup (SubProof fs ps l) n

-----------------------------------------------------------------------------
data Action where
  PointerDown :: Int -> PointerEvent -> Action
  PointerUp :: PointerEvent -> Action
  HandlePointer :: PointerEvent -> Action
  deriving (Show, Eq)

-----------------------------------------------------------------------------
-- TODO model needs to keep track of every elements' x and y coords. (or does it? maybe we can visualize dragging a line just by some phantom object moving..)
-- actually, just keep track of current element, this can be an either (proof, line or formula) and then insert a phantom object into the proof tree.
data Model rule formula = Model
  { _cursorX :: Double,
    _cursorY :: Double,
    _active :: Bool,
    _proof :: Proof rule formula
  }
  deriving (Eq)

active :: Miso.Lens.Lens (Model rule formula) Bool
active = Miso.Lens.lens (._active) $ \model a -> model {_active = a}

cursorX :: Miso.Lens.Lens (Model rule formula) Double
cursorX = Miso.Lens.lens (._cursorX) $ \model x -> model {_cursorX = x}

cursorY :: Miso.Lens.Lens (Model rule formula) Double
cursorY = Miso.Lens.lens (._cursorY) $ \model y -> model {_cursorY = y}

proof :: Miso.Lens.Lens (Model rule formula) (Proof rule formula)
proof = Miso.Lens.lens (._proof) $ \model p -> model {_proof = p}