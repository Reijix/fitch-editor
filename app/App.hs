-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
module App (runApp) where

import Control.Monad (when)
import qualified Data.Map as M
import Miso
  ( App,
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
import qualified Miso.Svg.Element as S
import qualified Miso.Svg.Property as SP
import Syntax

-----------------------------------------------------------------------------

-- | Test of Haddock
runApp :: forall rule formula. (Eq formula) => (Eq rule) => (Show rule) => (Show formula) => Model rule formula -> IO ()
runApp emptyModel = run $ startApp app
  where
    app :: App (Model rule formula) Action
    app =
      (component emptyModel updateModel viewModel)
        { subs = [mouseSub HandlePointer],
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
          cursor_x Miso.Lens..= x
          cursor_y Miso.Lens..= y
          -- io_ $ do consoleLog "pointer"
      )
updateModel (PointerDown n _) = do
  io_ $ consoleLog $ ms n
  active Miso.Lens..= True
updateModel (PointerUp _) = active Miso.Lens..= False

-----------------------------------------------------------------------------

viewModel :: forall rule formula. (Show rule) => (Show formula) => (Model rule formula) -> View (Model rule formula) Action
viewModel (Model x y _ prf) =
  H.div_
    [E.onPointerUp PointerUp]
    [ -- H.h1_ [] ["Fitch Editor"],
      H.svg_
        [ CSS.style_
            [ CSS.borderStyle "solid",
              CSS.height "700px",
              CSS.width "100%"
            ]
        ]
        -- TODO use foreignObject for lines, and then use onInput
        -- https://github.com/haskell-miso/miso-todomvc/blob/bc133d50971e18c137fbe3e2d0e05bc1a6b5c231/src/Main.hs#L310
        [ S.foreignObject_ [SP.height_ "1em", SP.width_ "5em", SP.x_ $ ms 200, SP.y_ $ ms 200] [H.input_ [HP.placeholder_ "asd"]],
          text_
            [ SP.x_ $ ms (x - 50),
              SP.y_ $ ms (y - 52)
            ]
            [ text $ ms $ show (round x :: Integer, round y :: Integer)
            ],
          S.g_
            []
            [ S.rect_
                [ SP.x_ "0",
                  SP.y_ "0",
                  CSS.style_ [CSS.fill "red"],
                  SP.width_ "100px",
                  SP.height_ "33%"
                ],
              S.rect_
                [ SP.x_ "0",
                  SP.y_ "33%",
                  CSS.style_ [CSS.fill "yellow"],
                  SP.width_ "100px",
                  SP.height_ "33%"
                ],
              S.rect_
                [ SP.x_ "0",
                  SP.y_ "66%",
                  CSS.style_ [CSS.fill "orange"],
                  SP.width_ "100px",
                  SP.height_ "33%"
                ]
            ],
          S.g_
            []
            [viewProof 0 (100, 50) prf]
        ]
    ]

-----------------------------------------------------------------------------