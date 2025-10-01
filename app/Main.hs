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
import qualified Miso.Html.Property as HP
import Miso.Lens (Lens, lens, this, use, (.=), (^.))
import Miso.Svg (text_, tspan_)
import qualified Miso.Svg.Element as S
import qualified Miso.Svg.Property as SP

-----------------------------------------------------------------------------
#if WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
main :: IO ()
main =
  run $ startApp app

-----------------------------------------------------------------------------

-- | Component definition (uses 'component' smart constructor)
app :: App Model Action
app =
  (component emptyModel updateModel viewModel)
    { subs = [mouseSub HandlePointer],
      events = M.fromList [("pointerdown", False), ("pointerup", False)]
    }

-----------------------------------------------------------------------------
emptyModel :: Model
emptyModel = Model {_x = 0.0, _y = 0.0, _active = False}

-----------------------------------------------------------------------------
updateModel :: Action -> Effect ROOT Model Action -- TODO is ROOT correct??
-- updateModel (HandlePointer pointer) = this .= client pointer
updateModel (HandlePointer pointer) =
  use active >>= \a ->
    when
      a
      ( do
          let (x, y) = client pointer
          curr_x .= x
          curr_y .= y
          io_ $ do consoleLog "pointer"
      )
updateModel (PointerDown _) = active .= True
updateModel (PointerUp _) = active .= False

-----------------------------------------------------------------------------
data Action
  = PointerDown PointerEvent
  | PointerUp PointerEvent
  | HandlePointer PointerEvent
  deriving (Show, Eq)

-----------------------------------------------------------------------------
data Model = Model {_x :: Double, _y :: Double, _active :: Bool} deriving (Eq)

active :: Lens Model Bool
active = lens _active $ \model n -> model {_active = n}

curr_x :: Lens Model Double
curr_x = lens _x $ \model n -> model {_x = n}

curr_y :: Lens Model Double
curr_y = lens _y $ \model n -> model {_y = n}

data Formula = Formula -- TODO

data Rule = Rule -- TODO

data Line = Line Formula Rule

data Proof = ProofLine Line | SubProof [Formula] [Proof] Line

-- disable text-highlighting during drag and drop. `preventDefault`
onPD :: (PointerEvent -> Action) -> Attribute Action
onPD f =
  onWithOptions
    preventDefault
    "pointerdown"
    pointerDecoder
    (\action _ -> f action)

-----------------------------------------------------------------------------
viewLine :: MisoString -> Line -> View Model Action
viewLine dy (Line _ _) = text_ [ SP.x_ "50", SP.y_ "50", SP.dy_ dy ] [tspan_ [ ] ["FORMULA"], tspan_ [ SP.dx_ "30px" ] ["RULE"]]

viewModel :: Model -> View Model Action
viewModel (Model x y _) =
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
            [onPD PointerDown]
            [ S.rect_
                [ SP.x_ $ ms (x - 50),
                  SP.y_ $ ms (y - 50),
                  CSS.style_
                    [ CSS.fill "yellow",
                      CSS.stroke "purple",
                      CSS.strokeWidth "2"
                    ],
                  SP.width_ "100px",
                  SP.height_ "100px"
                ]
            ],
          S.g_
            [onPD PointerDown]
            [ viewLine "0em" (Line Formula Rule), viewLine "1em" (Line Formula Rule), viewLine "2em" (Line Formula Rule)
            ]
        ]
    ]

-----------------------------------------------------------------------------