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
import qualified Miso.Html.Element as H
import Miso.Html.Event as E (onPointerUp)
import qualified Miso.Html.Property as HP
import Miso.Lens (use, (.=))
import Miso.Svg (text_)
import Syntax
import Views

-----------------------------------------------------------------------------

-- | Test of Haddock
runApp :: forall rule formula. (Eq formula) => (Eq rule) => (Show rule) => (Show formula) => Model rule formula -> IO ()
runApp emptyModel = run $ startApp app
  where
    app :: App (Model rule formula) Action
    app =
      (component emptyModel updateModel viewModel)
        { subs = [mouseSub HandlePointer],
          styles = [Href "assets/style.css"],
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
viewModel :: forall rule formula. (Show rule) => (Show formula) => Model rule formula -> View (Model rule formula) Action
viewModel (Model x y _ prf) =
  H.div_
    [E.onPointerUp PointerUp]
    [viewProof 0 (100, 50) prf]

-----------------------------------------------------------------------------