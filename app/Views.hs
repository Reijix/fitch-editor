module Views where

import qualified Data.List as L
import Miso
  ( Attribute,
    MisoString,
    PointerEvent,
    View,
    emptyDecoder,
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

viewDragIcon :: View (Model formula rule) Action
viewDragIcon = H.img_ [HP.src_ "draggable.svg", HP.height_ "16"]

-----------------------------------------------------------------------------
onDrop :: Action -> Attribute Action
onDrop f = onWithOptions preventDefault "drop" emptyDecoder $ \_ _ -> f

onDragEnter :: Action -> Attribute Action
onDragEnter f = onWithOptions preventDefault "dragEnter" emptyDecoder $ \_ _ -> f

-- disable text-highlighting during drag and drop. `preventDefault`
onPD :: (PointerEvent -> Action) -> Attribute Action
onPD f =
  onWithOptions
    preventDefault
    "pointerdown"
    pointerDecoder
    (\action _ -> f action)

-----------------------------------------------------------------------------
-- onDragOver :: action -> Attribute action
-- onDragOver = onDragOverWithOptions preventDefault

-- -----------------------------------------------------------------------------
-- onDragEnter :: action -> Attribute action
-- onDragEnter = onDragEnterWithOptions preventDefault

-- -----------------------------------------------------------------------------
-- onDragLeave :: action -> Attribute action
-- onDragLeave = onDragLeaveWithOptions preventDefault

-----------------------------------------------------------------------------

-- VIEWS
lineContainer ::
  forall formula rule.
  (Show formula) =>
  (Show rule) =>
  MisoString -> View (Model formula rule) Action
lineContainer s = H.p_ [HP.draggable_ True] [viewDragIcon, text s]

viewLine ::
  forall formula rule.
  (Show formula) =>
  (Show rule) =>
  Int -> Either (Assumption formula) (Derivation formula rule) -> View (Model formula rule) Action
viewLine n (Left f) = lineContainer $ ms $ show f ++ show n
viewLine n (Right (Derivation f r _)) = lineContainer $ ms $ show f ++ show r ++ show n

viewProof ::
  forall formula rule.
  (Show formula) =>
  (Show rule) =>
  Int -> (Int, Int) -> Proof formula rule -> View (Model formula rule) Action
viewProof n (x, y) p = snd $ _viewProof n 0 p
  where
    _viewProof :: Int -> Int -> Proof formula rule -> (Int, View (Model formula rule) Action)
    _viewProof n dy (PhantomProof m) = (n, H.div_ [] $ L.replicate m $ H.p_ [] [""])
    _viewProof n dy (ProofLine d) = (dy + 1, viewLine n (Right d))
    _viewProof n dy (SubProof fs ps d) = (dy + length allLines, H.div_ [] allLines)
      where
        allLines :: [View (Model formula rule) Action]
        allLines = do
          let (acc', fs') = L.mapAccumL (\acc f -> (acc + 1, viewLine (n + acc) (Left f))) 0 fs
          let (acc'', ps') = L.mapAccumL (\acc p -> _viewProof (n + acc) (dy + acc) p) acc' ps
          fs' ++ ps' ++ [viewLine (n + acc'') (Right d)]

-----------------------------------------------------------------------------
toEm :: Int -> MisoString
toEm n = ms (show n ++ "em")