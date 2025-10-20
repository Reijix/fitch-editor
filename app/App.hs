module App (runApp) where

import Control.Monad (when)
import qualified Data.Map as M
import Miso
  ( App,
    CSS (Href),
    Component (events, styles, subs),
    Effect,
    PointerEvent (client),
    ROOT,
    View,
    component,
    consoleLog,
    io_,
    mouseSub,
    ms,
    run,
    startApp,
    text,
  )
import qualified Miso.CSS as CSS
import qualified Miso.CSS as HP
import Miso.CSS.Color (red)
import qualified Miso.Html.Element as H
import Miso.Html.Event as E (onPointerUp)
import qualified Miso.Html.Property as HP
import Miso.Lens (use, (.=))
import Miso.Svg (text_)
import Syntax
import Views

-----------------------------------------------------------------------------

-- | Test of Haddock
runApp ::
  forall formula rule.
  (Eq formula) =>
  (Show formula) =>
  (Eq rule) =>
  (Show rule) =>
  Model rule formula ->
  IO ()
runApp emptyModel = run $ startApp app
  where
    app :: App (Model rule formula) Action
    app =
      (component emptyModel updateModel viewModel)
        { subs = [mouseSub HandlePointer],
          styles = [Href "style.css"],
          events = M.fromList [("pointerdown", False), ("pointerup", False), ("mouseover", False)]
        }

-----------------------------------------------------------------------------
updateModel :: Action -> Effect ROOT (Model rule formula) Action -- TODO is ROOT correct??
-- updateModel (HandlePointer pointer) = this .= client pointer
updateModel (HandlePointer pointer) =
  Miso.Lens.use active >>= \a ->
    when
      a
      ( do
          let (x, y) = client pointer
          cursorX Miso.Lens..= x
          cursorY Miso.Lens..= y
          -- io_ $ do consoleLog "pointer"
      )
updateModel (PointerDown n _) = do
  io_ $ consoleLog $ ms n
  active Miso.Lens..= True
updateModel (PointerUp _) = active Miso.Lens..= False

-----------------------------------------------------------------------------
viewModel ::
  forall formula rule.
  (Show formula) =>
  (Show rule) =>
  Model rule formula ->
  View (Model rule formula) Action
viewModel (Model x y _ prf) =
  H.div_
    [E.onPointerUp PointerUp]
    [ H.p_ [] [text $ ms $ show (round x :: Integer, round y :: Integer)],
      viewProof 0 (100, 50) prf,
      H.div_ [HP.style_ [HP.backgroundColor red, HP.maxWidth "100px", HP.minHeight "100px"], HP.draggable_ True] [H.p_ [HP.draggable_ True] ["drag1"], H.p_ [HP.draggable_ True] ["drag2"]]
    ]

-----------------------------------------------------------------------------