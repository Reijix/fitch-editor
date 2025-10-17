module Syntax
  ( RuleSpec (..),
    Proof (..),
    Assumption,
    Derivation (..),
    pLength,
    pLookup,
    Model (..),
    Action (..),
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

data RuleSpec formula rule = RuleSpec rule [Either formula (Proof formula rule)] formula deriving (Show, Eq)

data Reference where
  -- | Referencing a single line
  LineReference :: Int -> Reference
  -- | Referencing a subproof
  ProofReference :: Int -> Int -> Reference
  deriving (Show, Eq)

type Assumption formula = formula

data Derivation formula rule = Derivation formula rule [Reference] deriving (Show, Eq)

-- TODO add phantom proof/line, idea: Phantom Int (Int gives number of lines that are phantomed)
data Proof formula rule where
  PhantomProof :: Int -> Proof formula rule
  ProofLine :: Derivation formula rule -> Proof formula rule
  SubProof :: [Assumption formula] -> [Proof formula rule] -> Derivation formula rule -> Proof formula rule
  deriving (Show, Eq)

pLength :: Proof formula rule -> Int
pLength (PhantomProof n) = n
pLength (ProofLine l) = 1
pLength (SubProof fs ps _) = foldr (\p n -> pLength p + n) (L.length fs + 1) ps

pLookup :: Proof formula rule -> Int -> Either (Assumption formula) (Derivation formula rule)
pLookup (PhantomProof _) _ = error "Tried (!!) on PhantomProof"
pLookup (ProofLine d) 0 = Right d
pLookup (ProofLine _) _ = error "Tried (!!) on ProofLine with n > 0"
pLookup (SubProof fs _ _) n | n < L.length fs = Left $ fs L.!! n
pLookup (SubProof _ [] l) 0 = Right l
pLookup (SubProof _ [] _) _ = error "Tried (!!) on SubProof fs [] l with n > 0"
pLookup (SubProof fs (p : _) _) n | n < pLength p = pLookup p n
pLookup (SubProof fs (_ : ps) l) n = pLookup (SubProof fs ps l) n

-----------------------------------------------------------------------------
data Action where
  PointerDown :: Int -> PointerEvent -> Action
  PointerUp :: PointerEvent -> Action
  HandlePointer :: PointerEvent -> Action
  Drop :: Action
  DragEnter :: Action
  DragLeave :: Action
  DragOver :: Action
  deriving (Show, Eq)

-----------------------------------------------------------------------------
-- actually, just keep track of current element, this can be an either (proof, line or formula) and then insert a phantom object into the proof tree.
data Model formula rule = Model
  { _cursorX :: Double,
    _cursorY :: Double,
    _active :: Bool,
    _proof :: Proof formula rule
  }
  deriving (Eq)

active :: Miso.Lens.Lens (Model formula rule) Bool
active = Miso.Lens.lens (._active) $ \model a -> model {_active = a}

cursorX :: Miso.Lens.Lens (Model formula rule) Double
cursorX = Miso.Lens.lens (._cursorX) $ \model x -> model {_cursorX = x}

cursorY :: Miso.Lens.Lens (Model formula rule) Double
cursorY = Miso.Lens.lens (._cursorY) $ \model y -> model {_cursorY = y}

proof :: Miso.Lens.Lens (Model formula rule) (Proof formula rule)
proof = Miso.Lens.lens (._proof) $ \model p -> model {_proof = p}