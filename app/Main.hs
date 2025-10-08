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
emptyModel = Model {_cursor_x = 0.0, _cursor_y = 0.0, _active = False, _proof = EMPTYP}

-----------------------------------------------------------------------------
updateModel :: Action -> Effect ROOT Model Action -- TODO is ROOT correct??
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
data Action
  = PointerDown Int PointerEvent
  | PointerUp PointerEvent
  | HandlePointer PointerEvent
  deriving (Show, Eq)

-----------------------------------------------------------------------------
data Model = Model
  { _cursor_x :: Double
  , _cursor_y :: Double
  , _active :: Bool
  , _proof :: Proof
  } deriving (Eq)

active :: Miso.Lens.Lens Model Bool
active = Miso.Lens.lens _active $ \model a -> model {_active = a}

cursor_x :: Miso.Lens.Lens Model Double
cursor_x = Miso.Lens.lens _cursor_x $ \model x -> model {_cursor_x = x}

cursor_y :: Miso.Lens.Lens Model Double
cursor_y = Miso.Lens.lens _cursor_y $ \model y -> model {_cursor_y = y}

proof :: Miso.Lens.Lens Model Proof
proof = Miso.Lens.lens _proof $ \model p -> model {_proof = p}

data Formula = Formula deriving (Show, Eq) -- TODO

data Rule = Rule deriving (Show, Eq) -- TODO

data Line = Line Formula Rule deriving Eq

data Proof = EMPTYP | ProofLine Line | SubProof [Formula] [Proof] Line deriving Eq

pLength :: Proof -> Int
pLength EMPTYP             = 0
pLength (ProofLine l)      = 1
pLength (SubProof fs ps _) = foldr (\p n -> pLength p + n) (L.length fs + 1) ps

pLookup :: Proof -> Int -> Either Line Formula
pLookup EMPTYP n                                = error "Tried (!!) on EMPTYP"
pLookup (ProofLine l) 0                         = Left l
pLookup (ProofLine _) _                         = error "Tried (!!) on ProofLine with n > 0"
pLookup (SubProof fs _ _) n | n < L.length fs   = Right $ fs L.!! n
pLookup (SubProof _ [] l) 0                     = Left l
pLookup (SubProof _ [] _) _                     = error "Tried (!!) on SubProof fs [] l with n > 0"
pLookup (SubProof fs (p:_) _) n | n < pLength p = pLookup p n
pLookup (SubProof fs (_:ps) l) n | otherwise    = pLookup (SubProof fs ps l) n


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

exProof :: Proof
exProof = SubProof [Formula, Formula] [ProofLine (Line Formula Rule)] (Line Formula Rule)
-----------------------------------------------------------------------------
-- VIEWS
viewLine :: Int -> Int -> Line -> View Model Action
viewLine n dy (Line f r) = text_ [ SP.x_ "50", SP.y_ "50", SP.dy_ $ toEm dy, onPD (PointerDown n) ] [tspan_ [ ] [ text $ ms $ (show f ++ show n) ], tspan_ [ SP.dx_ "30px" ] [ text $ ms $ (show r ++ show n) ]]

viewRule :: Int -> Int -> Rule -> View Model Action
viewRule n dy r = text_ [ SP.x_ "50", SP.y_ "50", SP.dy_ $ toEm dy, onPD (PointerDown n) ] [ text $ ms $ show r ]

viewFormula :: Int -> Int -> Formula -> View Model Action
viewFormula n dy f = text_ [ SP.x_ "50", SP.y_ "50", SP.dy_ $ toEm dy, onPD (PointerDown n) ] [ text $ ms $ show f ]

viewProof :: Int -> (Int, Int) -> Proof -> View Model Action
viewProof n (x,y) p = snd $ _viewProof n 0 p
  where 
    _viewProof :: Int -> Int -> Proof -> (Int, View Model Action)
    _viewProof n _ EMPTYP = (0, S.g_ [] [])
    _viewProof n dy (ProofLine l) = (dy + 1, viewLine n dy l)
    _viewProof n dy (SubProof fs ps l) = (dy + length allLines, S.g_ [] allLines)
      where
        allLines :: [View Model Action]
        allLines = do
          let (acc', fs') = L.mapAccumL (\acc f  -> (acc + 1, viewFormula (n + acc) (dy + acc) f)) 0 fs
          let (acc'', ps') = L.mapAccumL (\acc p -> _viewProof (n + acc) (dy + acc) p) acc' ps
          fs' ++ ps' ++ [viewLine (n + acc'') (dy + acc'') l]

viewModel :: Model -> View Model Action
viewModel (Model x y _ _) =
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
            [onPD (PointerDown 100)]
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
              []
              [viewProof 0 (50, 50) exProof]
          -- S.g_
          --   [onPD PointerDown]
          --   [ viewLine 0 (Line Formula Rule), viewLine "1em" (Line Formula Rule), viewLine "2em" (Line Formula Rule)
          --   ]
        ]
    ]

-----------------------------------------------------------------------------