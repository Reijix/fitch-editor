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
import Miso.Property (textProp)
import Miso.Svg (text_, tspan_)
import qualified Miso.Svg.Element as S
import qualified Miso.Svg.Property as SP
import Syntax

href_ :: MisoString -> Attribute action
href_ = textProp "href"

viewDragIcon :: (Int, Int) -> Int -> View (Model rule formula) Action
viewDragIcon (x, y) dy = S.image_ [href_ "draggable.svg", SP.width_ "1ex", SP.height_ "1ic", SP.x_ $ ms x, SP.y_ $ ms "calc(100px + 5em)"]

-- VIEWS
viewLine ::
  forall rule formula.
  (Show rule) =>
  (Show formula) =>
  Int -> (Int, Int) -> Int -> Line rule formula -> View (Model rule formula) Action
viewLine n (x, y) dy (Line f r) = S.g_ [SP.x_ $ ms x, SP.y_ $ ms y] [viewDragIcon (x, y) dy, text_ [SP.x_ $ ms x, SP.y_ $ ms y, SP.dy_ $ toEm dy, onPD (PointerDown n)] [tspan_ [] [text $ ms (show f ++ show n)], tspan_ [SP.dx_ "30px"] [text $ ms (show r ++ show n)]]]

viewRule ::
  forall rule formula.
  (Show rule) =>
  (Show formula) =>
  Int -> (Int, Int) -> Int -> RuleSpec rule formula -> View (Model rule formula) Action
viewRule n (x, y) dy r = text_ [SP.x_ $ ms x, SP.y_ $ ms y, SP.dy_ $ toEm dy, onPD (PointerDown n)] [text $ ms $ show r]

viewFormula ::
  forall rule formula.
  (Show rule) =>
  (Show formula) =>
  Int -> (Int, Int) -> Int -> formula -> View (Model rule formula) Action
viewFormula n (x, y) dy f = text_ [SP.x_ $ ms x, SP.y_ $ ms y, SP.dy_ $ toEm dy, onPD (PointerDown n)] [text $ ms $ show f]

-- TODO intersperse every line with rectangles that have a "onmouseover" such that they turn red when action_ is true and hovered over.
-- TODO wrap each proof in g_ for grabbing it.
viewProof ::
  forall rule formula.
  (Show rule) =>
  (Show formula) =>
  Int -> (Int, Int) -> Proof rule formula -> View (Model rule formula) Action
viewProof n (x, y) p = snd $ _viewProof n 0 p
  where
    _viewProof :: Int -> Int -> Proof rule formula -> (Int, View (Model rule formula) Action)
    _viewProof n dy (PhantomProof m) = (n, S.g_ [] $ L.replicate m $ text_ [SP.x_ $ ms x, SP.y_ $ ms y, SP.dy_ $ toEm dy] [""])
    _viewProof n dy (ProofLine l) = (dy + 1, viewLine n (x, y) dy l)
    _viewProof n dy (SubProof fs ps l) = (dy + length allLines, S.g_ [] allLines)
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