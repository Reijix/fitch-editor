-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
module Main where

import Control.Monad (when)
import qualified Data.Map as M
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
import qualified Miso.Html.Element as H
import Miso.Html.Event as E
import Miso.Lens (Lens, lens, this, use, (.=), (^.))
import Miso.Svg (text_, tspan_)
import qualified Miso.Svg.Element as S
import qualified Miso.Svg.Property as SP
import qualified Data.List as L
import Data.Either
import Syntax

-----------------------------------------------------------------------------
#if WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
main :: IO ()
main =
  run $ startApp app

-----------------------------------------------------------------------------

data Rule = Rule deriving (Show, Eq)
data Formula = Formula deriving (Show, Eq)

-- | Component definition (uses 'component' smart constructor)
app :: App (Model Rule Formula) Action
app =
  (component emptyModel updateModel viewModel)
    { subs = [mouseSub HandlePointer],
      events = M.fromList [("pointerdown", False), ("pointerup", False), ("mouseover", False)]
    }

-----------------------------------------------------------------------------
emptyModel :: (Model Rule Formula)
emptyModel = Model {_cursor_x = 50, _cursor_y = 52, _active = False, _proof = exProof}

-----------------------------------------------------------------------------
updateModel :: Action -> Effect ROOT (Model Rule Formula) Action -- TODO is ROOT correct??
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

exProof :: (Proof Rule Formula)
exProof = SubProof [Formula, Formula] [ProofLine (Line Formula $ RuleSpec Rule [] Formula)] (Line Formula $ RuleSpec Rule [] Formula)
-----------------------------------------------------------------------------

viewModel :: (Model Rule Formula) -> View (Model Rule Formula) Action
viewModel (Model x y act prf) =
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
        [ text_
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
                  CSS.style_ [ CSS.fill "red" ],
                  SP.width_ "100px",
                  SP.height_ "33%"
                ],
                S.rect_ 
                [ SP.x_ "0",
                  SP.y_ "33%",
                  CSS.style_ [ CSS.fill "yellow" ],
                  SP.width_ "100px",
                  SP.height_ "33%"
                ],
                S.rect_ 
                [ SP.x_ "0",
                  SP.y_ "66%",
                  CSS.style_ [ CSS.fill "orange" ],
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