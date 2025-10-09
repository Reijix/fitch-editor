{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Syntax where

  import qualified Data.List as L
  import Miso
    ( App,
      MisoString,
      Attribute,
      Component (events, subs),
      Effect,
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
  import Miso.Svg (text_, tspan_)
  import qualified Miso.Svg.Element as S
  import qualified Miso.Svg.Property as SP
  import Miso.Lens (Lens, lens, this, use, (.=), (^.))

  data RuleSpec rule formula = RuleSpec rule [ Either formula (Proof rule formula) ] formula deriving (Show, Eq)

  data Line rule formula = Line formula (RuleSpec rule formula) deriving (Show, Eq)

  -- TODO add phantom proof/line, idea: Phantom Int (Int gives number of lines that are phantomed)
  data Proof rule formula = ProofLine (Line rule formula) | SubProof [formula] [Proof rule formula] (Line rule formula) deriving (Show, Eq)

  pLength :: Proof rule formula -> Int
  pLength (ProofLine l)      = 1
  pLength (SubProof fs ps _) = foldr (\p n -> pLength p + n) (L.length fs + 1) ps

  pLookup :: Proof rule formula -> Int -> Either (Line rule formula) formula
  pLookup (ProofLine l) 0                         = Left l
  pLookup (ProofLine _) _                         = error "Tried (!!) on ProofLine with n > 0"
  pLookup (SubProof fs _ _) n | n < L.length fs   = Right $ fs L.!! n
  pLookup (SubProof _ [] l) 0                     = Left l
  pLookup (SubProof _ [] _) _                     = error "Tried (!!) on SubProof fs [] l with n > 0"
  pLookup (SubProof fs (p:_) _) n | n < pLength p = pLookup p n
  pLookup (SubProof fs (_:ps) l) n | otherwise    = pLookup (SubProof fs ps l) n

  -- VIEWS
  viewLine :: forall rule formula . Show rule => Show formula => Int -> (Int, Int) -> Int -> (Line rule formula) -> View (Model rule formula) Action
  viewLine n (x,y) dy (Line f r) = text_ [ SP.x_ $ ms x, SP.y_ $ ms y, SP.dy_ $ toEm dy, onPD (PointerDown n) ] [tspan_ [ ] [ text $ ms $ (show f ++ show n) ], tspan_ [ SP.dx_ "30px" ] [ text $ ms $ (show r ++ show n) ]]

  viewRule :: forall rule formula . Show rule => Show formula => Int -> (Int, Int) -> Int -> (RuleSpec rule formula) -> View (Model rule formula) Action
  viewRule n (x,y) dy r = text_ [ SP.x_ $ ms x, SP.y_ $ ms y, SP.dy_ $ toEm dy, onPD (PointerDown n) ] [ text $ ms $ show r ]

  viewFormula :: forall rule formula . Show rule => Show formula => Int -> (Int, Int) -> Int -> formula -> View (Model rule formula) Action
  viewFormula n (x,y) dy f = text_ [ SP.x_ $ ms x, SP.y_ $ ms y, SP.dy_ $ toEm dy, onPD (PointerDown n) ] [ text $ ms $ show f ]

  -- TODO intersperse every line with rectangles that have a "onmouseover" such that they turn red when action_ is true and hovered over.
  -- TODO wrap each proof in g_ for grabbing it.
  viewProof :: forall rule formula . Show rule => Show formula => Int -> (Int, Int) -> Proof rule formula -> View (Model rule formula) Action
  viewProof n (x,y) p = snd $ _viewProof n 0 p
    where 
      _viewProof :: Int -> Int -> Proof rule formula -> (Int, View (Model rule formula) Action)
      _viewProof n dy (ProofLine l) = (dy + 1, viewLine n (x,y) dy l)
      _viewProof n dy (SubProof fs ps l) = (dy + length allLines, S.g_ [] allLines)
        where
          allLines :: [View (Model rule formula) Action]
          allLines = do
            let (acc', fs') = L.mapAccumL (\acc f  -> (acc + 1, viewFormula (n + acc) (x,y) (dy + acc) f)) 0 fs
            let (acc'', ps') = L.mapAccumL (\acc p -> _viewProof (n + acc) (dy + acc) p) acc' ps
            fs' ++ ps' ++ [viewLine (n + acc'') (x,y) (dy + acc'') l]

  -- disable text-highlighting during drag and drop. `preventDefault`
  onPD :: (PointerEvent -> Action) -> Attribute Action
  onPD f =
    onWithOptions
      preventDefault
      "pointerdown"
      pointerDecoder
      (\action _ -> f action)

  -----------------------------------------------------------------------------
  toEm :: Int -> MisoString
  toEm n = ms (show n ++ "em")

  -----------------------------------------------------------------------------
  data Action
    = PointerDown Int PointerEvent
    | PointerUp PointerEvent
    | HandlePointer PointerEvent
    deriving (Show, Eq)

  -----------------------------------------------------------------------------
  -- TODO model needs to keep track of every elements' x and y coords. (or does it? maybe we can visualize dragging a line just by some phantom object moving..)
  -- actually, just keep track of current element, this can be an either (proof, line or formula) and then insert a phantom object into the proof tree.
  data Model rule formula = Model
    { _cursor_x :: Double
    , _cursor_y :: Double
    , _active :: Bool
    , _proof :: Proof rule formula
    } deriving (Eq)

  active :: Miso.Lens.Lens (Model rule formula) Bool
  active = Miso.Lens.lens _active $ \model a -> model {_active = a}

  cursor_x :: Miso.Lens.Lens (Model rule formula) Double
  cursor_x = Miso.Lens.lens _cursor_x $ \model x -> model {_cursor_x = x}

  cursor_y :: Miso.Lens.Lens (Model rule formula) Double
  cursor_y = Miso.Lens.lens _cursor_y $ \model y -> model {_cursor_y = y}

  proof :: Miso.Lens.Lens (Model rule formula) (Proof rule formula)
  proof = Miso.Lens.lens _proof $ \model p -> model {_proof = p}