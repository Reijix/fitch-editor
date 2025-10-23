module Syntax
  ( RuleSpec (..),
    Proof (..),
    Assumption,
    Derivation (..),
    lLength,
    lLookup,
    Model (..),
    Action (..),
    active,
    cursorX,
    cursorY,
    proof,
    DropLocation (..),
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
  ProofLine :: Derivation formula rule -> Proof formula rule
  SubProof :: [Assumption formula] -> [Proof formula rule] -> Derivation formula rule -> Proof formula rule
  deriving (Show, Eq)

insertAt :: a -> Int -> [a] -> Maybe [a]
insertAt x 0 xs = Just $ x : xs
insertAt x n [] = Nothing
insertAt x n (y : ys) = do
  ys' <- insertAt x (n - 1) ys
  Just $ y : ys'

removeAt :: Int -> [a] -> Maybe [a]
removeAt n [] = Nothing
removeAt n (x : xs)
  | n == 0 = Just xs
  | n > 0 = do
      xs' <- removeAt (n - 1) xs
      return $ x : xs'

lLength :: Proof formula rule -> Int
lLength (ProofLine l) = 1
lLength (SubProof fs ps _) = foldr (\p n -> lLength p + n) (L.length fs + 1) ps

lLookup :: Proof formula rule -> Int -> Either (Assumption formula) (Derivation formula rule)
lLookup (ProofLine d) 0 = Right d
lLookup (ProofLine _) _ = error "Tried (!!) on ProofLine with n > 0"
lLookup (SubProof fs _ _) n | n < L.length fs = Left $ fs L.!! n
lLookup (SubProof _ [] l) 0 = Right l
lLookup (SubProof _ [] _) _ = error "Tried (!!) on SubProof fs [] l with n > 0"
lLookup (SubProof fs (p : _) _) n | n < lLength p = lLookup p n
lLookup (SubProof fs (_ : ps) l) n = lLookup (SubProof fs ps l) n

lRemove :: Int -> Proof formula rule -> Maybe (Proof formula rule)
lRemove _ (ProofLine _) = Nothing
lRemove n p@(SubProof fs ps l) =
  maybe
    ( do
        ps' <- tryRemove n ps
        return $ SubProof fs ps' l
    )
    (\fs' -> Just $ SubProof fs' ps l)
    (removeAt n fs)
  where
    tryRemove :: Int -> [Proof formula rule] -> Maybe [Proof formula rule]
    tryRemove 0 ((ProofLine _) : ps) = Just ps
    tryRemove n (p@(SubProof {}) : ps) =
      maybe
        ( do
            ps' <- tryRemove (n - lLength p) ps
            return $ p : ps'
        )
        (\p' -> Just $ p' : ps)
        (lRemove n p)

lInsert :: Either (Assumption formula) (Derivation formula rule) -> Int -> Proof formula rule -> Maybe (Proof formula rule)
lInsert (Left _) n (ProofLine _) = Nothing
lInsert (Left a) n (SubProof as ps d) = maybe (tryInsert a (n - length as) ps >>= (\ps' -> return $ SubProof as ps' d)) (\as' -> return $ SubProof as' ps d) (insertAt a n as)
  where
    tryInsert :: Assumption formula -> Int -> [Proof formula rule] -> Maybe [Proof formula rule]
    tryInsert a n (p : ps) = maybe (tryInsert a (n - lLength p) ps >>= (\ps' -> return $ p : ps')) (\p' -> return $ p' : ps) (lInsert (Left a) n p)
lInsert (Right (Derivation {})) n (ProofLine _) = Nothing
lInsert (Right d) 0 (SubProof fs ps l) = Just $ SubProof fs (ProofLine d : ps) l
lInsert (Right d) n (SubProof fs (p : ps) l) =
  maybe
    ( do
        p' <- lInsert (Right d) (n - lLength p) (SubProof fs ps l)
        case p' of
          ProofLine _ -> error ""
          SubProof fs' ps' l' -> return $ SubProof fs' (p : ps') l'
    )
    (\p' -> return $ SubProof fs (p' : ps) l)
    (lInsert (Right d) n p)

-----------------------------------------------------------------------------
data DropLocation where
  Line :: Int -> DropLocation
  Proof :: Int -> DropLocation
  Bin :: DropLocation
  deriving (Show, Eq)

data Action where
  PointerDown :: Int -> PointerEvent -> Action
  PointerUp :: PointerEvent -> Action
  HandlePointer :: PointerEvent -> Action
  Drop :: DropLocation -> Action
  DragEnter :: Action
  DragLeave :: Action
  DragOver :: Action
  DragStart :: Action
  DragEnd :: Action
  Drag :: Action
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