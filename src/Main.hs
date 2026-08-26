-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Control.Monad                 (forM_, when)
import           Control.Monad.IO.Class         (liftIO)
import           Data.IORef
import qualified Data.Map.Strict as M
-----------------------------------------------------------------------------
import           Miso hiding ((!!))
import           Miso.Lens
import           Miso.Html
import           Miso.Html.Property
import           Miso.Canvas                    (Canvas)
import qualified Miso.Canvas as Canvas
import           Miso.String hiding (take, last, init, length)
import qualified Miso.CSS.Color as Color
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
-- | Each card is an independent little solar system, keyed by an
-- ever-increasing id so Add\/Remove never reuse a stale identity.
data Model = Model
  { _canvases :: [Int]
  , _planets  :: M.Map Int Int
  , _nextId   :: Int
  } deriving (Eq, Show)
-----------------------------------------------------------------------------
data Action
  = AddCanvas
  | RemoveCanvas
  | AddPlanet Int
  | RemovePlanet Int
  | InitCanvas Int DOMRef
  | CanvasReady Int DOMRef (IORef FpsState)
  | StopCanvas Int
-----------------------------------------------------------------------------
-- | Previous frame's timestamp and a smoothed fps reading. Lives outside
-- 'Model' — 'canvasSub' runs in a tight rAF loop that never touches the
-- vdom, so each card threads its own fps state through a plain 'IORef'
-- rather than dispatching through @update@.
data FpsState = FpsState
  { fpsLast     :: !Double
  , fpsSmoothed :: !Double
  }
-----------------------------------------------------------------------------
-- | A background star: x, y, radius. Generated once per card so the
-- field doesn't shimmer between frames.
data Star = Star !Double !Double !Double
-----------------------------------------------------------------------------
canvasPx :: Int
canvasPx = 240

canvasSize :: Double
canvasSize = fromIntegral canvasPx
-----------------------------------------------------------------------------
canvasSubKey :: Int -> MisoString
canvasSubKey cid = "canvas2d-" <> ms cid
-----------------------------------------------------------------------------
main :: IO ()
main = startApp defaultEvents app
  where
    app :: App Model Action
    app = component (Model [0] (M.singleton 0 defaultPlanetCount) 1) updateModel viewModel

    viewModel () () Model { _canvases = cs, _planets = ps } =
      div_
      [ class_ "app" ]
      [ h1_
        [ class_ "title" ]
        [ span_ [ class_ "title-emoji" ] [ "🍜 " ]
        , a_ [ href_ "https://github.com/haskell-miso/miso-canvas2d" ] [ "miso-canvas2d" ]
        ]
      , div_
        [ class_ "toolbar" ]
        [ button_ [ class_ "btn btn-primary", onClick AddCanvas ] [ "+ Add canvas" ]
        , button_ [ class_ "btn btn-danger", onClick RemoveCanvas ] [ "− Remove canvas" ]
        ]
      , div_
        [ class_ "grid" ]
        [ canvasCard cid (M.findWithDefault defaultPlanetCount cid ps) | cid <- cs ]
      ]
-----------------------------------------------------------------------------
-- | Two stacked canvases per card: a declarative 'Canvas.canvas' underneath
-- for everything that only changes when the planet count does (background,
-- nebula, stars, orbit rings — redrawn on model updates, not every frame),
-- and a 'canvasSub'-driven layer on top for what actually moves each frame
-- (planets, sun, fps badge), cleared to transparent so the background shows
-- through. Redrawing the static ~90% of the scene 60x/sec was the reason
-- fps dropped once several cards were on screen at once.
canvasCard :: Int -> Int -> View context Model Action
canvasCard cid k =
  div_
  [ class_ "card" ]
  [ div_
    [ class_ "canvas-stack" ]
    [ Canvas.canvas
      [ width_ (ms canvasPx), height_ (ms canvasPx), class_ "space-canvas" ]
      (\_ -> pure ())
      (\() -> drawBackground cid k (mkStars cid))
    , canvas_
      [ width_  (ms canvasPx)
      , height_ (ms canvasPx)
      , class_ "space-canvas"
      , onCreatedWith (InitCanvas cid)
      , onDestroyed (StopCanvas cid)
      ]
      []
    ]
  , div_
    [ class_ "card-toolbar" ]
    [ button_ [ class_ "icon-btn", onClick (RemovePlanet cid) ] [ "−" ]
    , span_ [ class_ "planet-count" ] [ text_ [ ms k <> " planets" ] ]
    , button_ [ class_ "icon-btn", onClick (AddPlanet cid) ] [ "+" ]
    ]
  ]
-----------------------------------------------------------------------------
-- | The eight real planets, in orbital order, with roughly-true relative
-- size and colour. Only 'Saturn' gets a ring. This caps how far Add\/Remove
-- can go — there just aren't more planets than this in our solar system.
data Planet = Planet
  { planetName   :: MisoString
  , planetColor  :: Color.Color
  , planetRadius :: Double
  , planetRing   :: Bool
  }

solarSystem :: [Planet]
solarSystem =
  [ Planet "Mercury" (Color.rgb 176 165 151) 3.0 False
  , Planet "Venus"   (Color.rgb 226 191 125) 5.0 False
  , Planet "Earth"   (Color.rgb 90 150 214)  5.2 False
  , Planet "Mars"    (Color.rgb 193 101 62)  4.0 False
  , Planet "Jupiter" (Color.rgb 216 178 130) 9.0 False
  , Planet "Saturn"  (Color.rgb 227 200 140) 8.0 True
  , Planet "Uranus"  (Color.rgb 150 211 214) 6.5 False
  , Planet "Neptune" (Color.rgb 78 100 209)  6.3 False
  ]

defaultPlanetCount :: Int
defaultPlanetCount = 4
-----------------------------------------------------------------------------
orbitRadius :: Int -> Double
orbitRadius n = 16 + fromIntegral n * 12

orbitPeriod :: Int -> Double
orbitPeriod n = 2200 + fromIntegral n * 650

-- | Each orbit gets its own inclination (as an ellipse rotation + squash),
-- mixing in the card's id as well as the orbit index — so planets don't
-- all sweep the same flat plane *and* different cards don't all show the
-- same fan of tilts. Both are pure functions, so background rings and the
-- moving planet stay in sync without sharing any extra state.
orbitTilt :: Int -> Int -> Double
orbitTilt cid n = fromIntegral (n - 1) * (pi / 6) + fromIntegral cid * 0.7

orbitSquash :: Int -> Int -> Double
orbitSquash cid n = 0.28 + 0.24 * abs (sin (fromIntegral (n * 7 + cid * 5) * 0.9))

-- | Where a planet sits on its tilted, squashed orbit at a given angle,
-- in canvas coordinates.
orbitPoint :: Double -> Double -> Int -> Int -> Double -> (Double, Double)
orbitPoint cx cy cid n angle =
  let r      = orbitRadius n
      tilt   = orbitTilt cid n
      squash = orbitSquash cid n
      lx     = r * cos angle
      ly     = r * sin angle * squash
      px     = cx + lx * cos tilt - ly * sin tilt
      py     = cy + lx * sin tilt + ly * cos tilt
  in (px, py)
-----------------------------------------------------------------------------
-- | Deterministic pseudo-random starfield, seeded by the card's id so
-- reloading doesn't reshuffle it and every card still looks different.
mkStars :: Int -> [Star]
mkStars seed = take 50 (go (frac (fromIntegral seed * 0.6180339887 + 0.1234)))
  where
    go s =
      let s1 = frac (s  * 12.9898 + 78.233)
          s2 = frac (s1 * 39.3468 + 11.135)
          s3 = frac (s2 * 26.7819 + 5.912)
      in Star (s1 * canvasSize) (s2 * canvasSize) (0.3 + s3 * 0.9) : go s3
    frac x = x - fromIntegral (floor x :: Int)
-----------------------------------------------------------------------------
-- | The static ~90% of a card's scene: backdrop, nebula, stars and the
-- tilted orbit rings. This only redraws when the vdom updates (i.e. when
-- planet counts change anywhere in the app), never on the animation
-- timer, so it costs nothing at 60fps regardless of how many cards exist.
drawBackground :: Int -> Int -> [Star] -> Canvas ()
drawBackground cid k starField = do
  let cx = canvasSize / 2
      cy = canvasSize / 2

  Canvas.fillStyle (Canvas.color (Color.rgb 6 8 20))
  Canvas.fillRect (0, 0, canvasSize, canvasSize)

  -- faint nebula clouds (soft, low-opacity solid discs)
  Canvas.fillStyle (Canvas.color (Color.rgba 124 108 240 0.06))
  Canvas.beginPath ()
  Canvas.arc (cx * 0.4, cy * 0.5, canvasSize * 0.5, 0, pi * 2)
  Canvas.fill ()
  Canvas.fillStyle (Canvas.color (Color.rgba 56 189 248 0.05))
  Canvas.beginPath ()
  Canvas.arc (cx * 1.6, cy * 1.5, canvasSize * 0.5, 0, pi * 2)
  Canvas.fill ()

  -- stars
  Canvas.fillStyle (Canvas.color (Color.rgba 255 255 255 0.85))
  forM_ starField $ \(Star sx sy sr) -> do
    Canvas.beginPath ()
    Canvas.arc (sx, sy, sr, 0, pi * 2)
    Canvas.fill ()

  -- tilted orbit rings
  Canvas.strokeStyle (Canvas.color (Color.rgba 255 255 255 0.18))
  Canvas.lineWidth 1.2
  forM_ [1 .. k] $ \n -> do
    Canvas.save ()
    Canvas.translate (cx, cy)
    Canvas.rotate (orbitTilt cid n)
    Canvas.scale (1, orbitSquash cid n)
    Canvas.beginPath ()
    Canvas.arc (0, 0, orbitRadius n, 0, pi * 2)
    Canvas.stroke ()
    Canvas.restore ()
-----------------------------------------------------------------------------
-- | The animated ~10%: @k@ orbiting planets on their tilted paths around a
-- glowing sun, plus an fps badge — one rAF-driven pass courtesy of
-- 'canvasSub', cleared to transparent so 'drawBackground' shows through.
-- 'timestamp' comes straight from the browser, bypassing the vdom entirely.
-- Every fill here uses a plain solid colour (plus 'Canvas.shadowBlur' for
-- glow) rather than a canvas gradient — gradients silently failed to take
-- effect on this build, leaving the *previous* frame's fillStyle in place.
drawScene
  :: Int
  -> IORef FpsState
  -> Double
  -> Model
  -> Canvas ()
drawScene cid fpsRef timestamp model = do
  let k  = M.findWithDefault defaultPlanetCount cid (_planets model)
      cx = canvasSize / 2
      cy = canvasSize / 2

  fps <- liftIO $ do
    FpsState lastT smoothed <- readIORef fpsRef
    let dt = timestamp - lastT
        smoothed'
          | lastT <= 0 || dt <= 0 = smoothed
          | otherwise = smoothed * 0.9 + (1000 / dt) * 0.1
    writeIORef fpsRef (FpsState timestamp smoothed')
    pure smoothed'

  Canvas.clearRect (0, 0, canvasSize, canvasSize)

  -- planets, inner ones orbiting faster than outer ones
  forM_ [1 .. k] $ \n -> do
    let angle  = (timestamp / orbitPeriod n) * 2 * pi
        (px, py) = orbitPoint cx cy cid n angle
        planet = solarSystem !! ((n - 1) `mod` length solarSystem)
        pr     = planetRadius planet

    when (planetRing planet) $ do
      Canvas.save ()
      Canvas.translate (px, py)
      Canvas.rotate (orbitTilt cid n)
      Canvas.scale (1, 0.38)
      Canvas.strokeStyle (Canvas.color (Color.rgba 227 200 140 0.75))
      Canvas.lineWidth 1.5
      Canvas.beginPath ()
      Canvas.arc (0, 0, pr * 1.8, 0, pi * 2)
      Canvas.stroke ()
      Canvas.restore ()

    Canvas.fillStyle (Canvas.color (planetColor planet))
    Canvas.shadowBlur 8
    Canvas.shadowColor (planetColor planet)
    Canvas.beginPath ()
    Canvas.arc (px, py, pr, 0, pi * 2)
    Canvas.fill ()
    Canvas.shadowBlur 0

  -- sun: soft bloom, then a sharp disc on top (kept small so it doesn't
  -- wash out the innermost orbit rings)
  Canvas.fillStyle (Canvas.color (Color.rgba 255 200 90 0.15))
  Canvas.beginPath ()
  Canvas.arc (cx, cy, 18, 0, pi * 2)
  Canvas.fill ()

  Canvas.fillStyle (Canvas.color (Color.rgb 255 205 90))
  Canvas.shadowBlur 16
  Canvas.shadowColor (Color.rgb 255 160 60)
  Canvas.beginPath ()
  Canvas.arc (cx, cy, 10, 0, pi * 2)
  Canvas.fill ()
  Canvas.shadowBlur 0

  -- per-canvas fps badge
  Canvas.fillStyle (Canvas.color (Color.rgba 0 0 0 0.45))
  Canvas.fillRect (6, 6, 58, 20)
  Canvas.fillStyle (Canvas.color (Color.rgb 74 222 128))
  Canvas.font "11px ui-monospace, monospace"
  Canvas.fillText (ms (round fps :: Int) <> " fps", 12, 20)
-----------------------------------------------------------------------------
updateModel
  :: Action
  -> Effect parent props Model Action
updateModel = \case
  AddCanvas -> do
    cid <- use nextId
    canvases %= (++ [cid])
    planets  %= M.insert cid defaultPlanetCount
    nextId   += 1
  RemoveCanvas -> do
    cs <- use canvases
    case cs of
      [] -> pure ()
      _  -> do
        let cid = last cs
        canvases %= init
        planets  %= M.delete cid
  AddPlanet cid ->
    planets %= M.adjust (\x -> min (length solarSystem) (x + 1)) cid
  RemovePlanet cid ->
    planets %= M.adjust (\x -> max 1 (x - 1)) cid
  InitCanvas cid domRef ->
    sync $ do
      fpsRef <- newIORef (FpsState 0 0)
      pure (CanvasReady cid domRef fpsRef)
  CanvasReady cid domRef fpsRef ->
    startSub (canvasSubKey cid) $ canvasSub domRef "2d" (drawScene cid fpsRef)
  StopCanvas cid ->
    stopSub (canvasSubKey cid)
-----------------------------------------------------------------------------
canvases :: Lens Model [Int]
canvases = lens _canvases (\m x -> m { _canvases = x })
-----------------------------------------------------------------------------
planets :: Lens Model (M.Map Int Int)
planets = lens _planets (\m x -> m { _planets = x })
-----------------------------------------------------------------------------
nextId :: Lens Model Int
nextId = lens _nextId (\m x -> m { _nextId = x })
-----------------------------------------------------------------------------
