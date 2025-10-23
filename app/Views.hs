module Views where

import qualified Data.List as L
import Miso
  ( Attribute,
    MisoString,
    PointerEvent,
    View,
    defaultOptions,
    emptyDecoder,
    focus,
    ms,
    onWithOptions,
    pointerDecoder,
    preventDefault,
    text,
  )
import qualified Miso.Html as HP
import qualified Miso.Html.Element as H
import Miso.Html.Event
import Miso.Html.Property (value_)
import qualified Miso.Html.Property as HP
import Miso.Lens
import Miso.Property (boolProp, textProp)
import Miso.Svg (onFocusOut, text_, tspan_)
import qualified Miso.Svg.Element as S
import qualified Miso.Svg.Property as SP
import Syntax

viewDragIcon :: View (Model formula rule) Action
viewDragIcon = H.img_ [HP.draggable_ False, HP.src_ "./draggable.svg", HP.height_ "16"]

inert_ :: Bool -> Attribute action
inert_ = boolProp "inert"

-----------------------------------------------------------------------------
-- disable text-highlighting during drag and drop. `preventDefault`
onPD :: (PointerEvent -> Action) -> Attribute Action
onPD f =
  onWithOptions
    preventDefault
    "pointerdown"
    pointerDecoder
    (\action _ -> f action)

-----------------------------------------------------------------------------
viewBin :: View (Model formula rule) Action
viewBin =
  H.div_
    [ onDragOverWithOptions preventDefault DragOver,
      onDragEnterWithOptions preventDefault DragEnter,
      onDragLeaveWithOptions preventDefault DragLeave,
      onDropWithOptions defaultOptions (Drop Bin),
      HP.class_ "bin"
    ]
    []

-- VIEWS
lineContainer ::
  forall formula rule.
  (Show formula) =>
  (Show rule) =>
  Model formula rule -> Bool -> Int -> MisoString -> View (Model formula rule) Action
lineContainer m isAssumption n s =
  H.div_
    [ HP.draggable_ True,
      HP.classList_ [("proof-line", True), ("draggable", True)],
      onDragStart DragStart,
      onDragEnd DragEnd,
      onDoubleClick (DoubleClick n),
      onFocusOut Blur
    ]
    [ H.input_ [inert_ (n /= (m ^. focusedLine)), HP.id_ . ms $ "proof-line" ++ show n, HP.classList_ [("proof-input", True), ("assumption", isAssumption)], HP.draggable_ False, onDragStartWithOptions preventDefault DragStart, value_ s]
    ]

viewLine ::
  forall formula rule.
  (Show formula) =>
  (Show rule) =>
  Model formula rule -> Int -> Either (Assumption formula) (Derivation formula rule) -> View (Model formula rule) Action
viewLine m n (Left f) = lineContainer m True n $ ms $ show f ++ show n
viewLine m n (Right (Derivation f r _)) = lineContainer m False n $ ms $ show f ++ show r ++ show n

viewProof ::
  forall formula rule.
  (Show formula) =>
  (Show rule) =>
  Model formula rule -> View (Model formula rule) Action
viewProof m = H.div_ [] [proofView]
  where
    (lineNos, proofView) = _viewProof 0 0 (m ^. proof)
    _viewProof :: Int -> Int -> Proof formula rule -> (Int, View (Model formula rule) Action)
    _viewProof n dy (ProofLine d) = (dy + 1, viewLine m n (Right d))
    _viewProof n dy (SubProof fs ps d) = (dy + length allLines, H.div_ [HP.class_ "subproof", HP.draggable_ True, onDragStart DragStart, onDragEnd DragEnd] allLines)
      where
        allLines :: [View (Model formula rule) Action]
        allLines = do
          let (acc', fs') = L.mapAccumL (\acc f -> (acc + 1, viewLine m (n + acc) (Left f))) 0 fs
          let (acc'', ps') = L.mapAccumL (\acc p -> _viewProof (n + acc) (dy + acc) p) acc' ps
          fs' ++ ps' ++ [viewLine m (n + acc'') (Right d)]

-----------------------------------------------------------------------------
toEm :: Int -> MisoString
toEm n = ms (show n ++ "em")