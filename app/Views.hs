module Views where

import qualified Data.List as L
import Miso
  ( Attribute,
    MisoString,
    PointerEvent,
    View,
    ms,
    onWithOptions,
    pointerDecoder,
    preventDefault,
    text,
  )
import qualified Miso.Html.Element as H
import qualified Miso.Html.Property as HP
import Miso.Property (textProp)
import Miso.Svg (text_, tspan_)
import qualified Miso.Svg.Element as S
import qualified Miso.Svg.Property as SP
import Syntax

href_ :: MisoString -> Attribute action
href_ = textProp "href"

-- TODO
viewDragIcon :: (Int, Int) -> Int -> View (Model rule formula) Action
viewDragIcon (x, y) dy = H.img_ [HP.src_ "draggable.svg", HP.height_ "16"]

-- VIEWS
-- TODO
viewLine ::
  forall rule formula.
  (Show rule) =>
  (Show formula) =>
  Int -> (Int, Int) -> Int -> Line rule formula -> View (Model rule formula) Action
viewLine n (x, y) dy (Line f r) =
  H.p_
    []
    [viewDragIcon (x, y) dy, text $ ms (show f ++ show n), text $ ms (show r ++ show n)]

-- TODO
viewRule ::
  forall rule formula.
  (Show rule) =>
  (Show formula) =>
  Int -> (Int, Int) -> Int -> RuleSpec rule formula -> View (Model rule formula) Action
viewRule n (x, y) dy r = H.p_ [onPD (PointerDown n)] [text $ ms $ show r]

-- TODO
viewFormula ::
  forall rule formula.
  (Show rule) =>
  (Show formula) =>
  Int -> (Int, Int) -> Int -> formula -> View (Model rule formula) Action
viewFormula n (x, y) dy f = H.p_ [onPD (PointerDown n)] [viewDragIcon (x, y) dy, text $ ms $ show f]

-- TODO
viewProof ::
  forall rule formula.
  (Show rule) =>
  (Show formula) =>
  Int -> (Int, Int) -> Proof rule formula -> View (Model rule formula) Action
viewProof n (x, y) p = snd $ _viewProof n 0 p
  where
    _viewProof :: Int -> Int -> Proof rule formula -> (Int, View (Model rule formula) Action)
    -- TODO empty lines
    _viewProof n dy (PhantomProof m) = (n, H.div_ [] $ L.replicate m $ H.p_ [] [""])
    -- TODO
    _viewProof n dy (ProofLine l) = (dy + 1, viewLine n (x, y) dy l)
    _viewProof n dy (SubProof fs ps l) = (dy + length allLines, H.div_ [] allLines)
      where
        allLines :: [View (Model rule formula) Action]
        allLines = do
          let (acc', fs') = L.mapAccumL (\acc f -> (acc + 1, viewFormula (n + acc) (x, y) (dy + acc) f)) 0 fs
          let (acc'', ps') = L.mapAccumL (\acc p -> _viewProof (n + acc) (dy + acc) p) acc' ps
          fs' ++ ps' ++ [viewLine (n + acc'') (x, y) (dy + acc'') l]

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