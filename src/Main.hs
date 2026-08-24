-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Control.Monad      (forM_, unless, when)
-----------------------------------------------------------------------------
import           Miso
import           Miso.Lens
import qualified Miso.Html.Element  as H
import           Miso.Html.Event    (onClick)
import qualified Miso.Html.Property as P
import           Miso.Canvas        (Canvas, CompositeOperation (..))
import qualified Miso.Canvas        as Canvas
import qualified Miso.CSS           as CSS
import           Miso.JSON          (withArray, withObject, (.:))
import           Miso.String        (ms)
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
-- | Logical (internal) canvas resolution; CSS scales it responsively.
side :: Double
side = 640
-----------------------------------------------------------------------------
center :: Double
center = side / 2
-----------------------------------------------------------------------------
-- | Gravitational parameter of the sun (canvas units).
mu :: Double
mu = 42000
-----------------------------------------------------------------------------
data Comet = Comet
  { cometX  :: !Double
  , cometY  :: !Double
  , cometVX :: !Double
  , cometVY :: !Double
  , cometHue :: !Int
  } deriving (Eq, Show)
-----------------------------------------------------------------------------
data Model = Model
  { _time     :: Double   -- ^ simulation clock (ms), only advances when running
  , _lastTick :: Double   -- ^ last rAF timestamp (ms)
  , _fps      :: Double   -- ^ exponentially smoothed frames per second
  , _planets  :: Int
  , _comets   :: [Comet]
  , _paused   :: Bool
  , _trails   :: Bool
  } deriving (Eq, Show)
-----------------------------------------------------------------------------
time' :: Lens Model Double
time' = lens _time $ \m x -> m { _time = x }

lastTick :: Lens Model Double
lastTick = lens _lastTick $ \m x -> m { _lastTick = x }

fps :: Lens Model Double
fps = lens _fps $ \m x -> m { _fps = x }

planets :: Lens Model Int
planets = lens _planets $ \m x -> m { _planets = x }

comets :: Lens Model [Comet]
comets = lens _comets $ \m x -> m { _comets = x }

paused :: Lens Model Bool
paused = lens _paused $ \m x -> m { _paused = x }

trails :: Lens Model Bool
trails = lens _trails $ \m x -> m { _trails = x }
-----------------------------------------------------------------------------
data Action
  = Tick Double
  | AddPlanet
  | RemovePlanet
  | TogglePause
  | ToggleTrails
  | ClearComets
  | LaunchComet (Double, Double)
-----------------------------------------------------------------------------
emptyModel :: Model
emptyModel = Model 0 0 60 5 [] False True
-----------------------------------------------------------------------------
main :: IO ()
main = startApp defaultEvents app
-----------------------------------------------------------------------------
app :: App Model Action
app = (component emptyModel updateModel viewModel)
  { subs = [ rAFSub Tick ]
  }
-----------------------------------------------------------------------------
updateModel :: Action -> Effect context props Model Action
updateModel = \case
  Tick stamp -> do
    prev <- use lastTick
    lastTick .= stamp
    let dt = min 64 (stamp - prev) -- clamp pauses / tab switches
    when (prev > 0 && dt > 0) $
      fps %= \old -> old * 0.95 + (1000 / dt) * 0.05
    isPaused <- use paused
    unless isPaused $ do
      time' += dt
      comets %= concatMap (stepComet dt)
  AddPlanet     -> planets %= min 8 . (+ 1)
  RemovePlanet  -> planets %= max 0 . subtract 1
  TogglePause   -> paused %= not
  ToggleTrails  -> trails %= not
  ClearComets   -> comets .= []
  LaunchComet p -> comets %= \cs -> take 24 (launchComet p : cs)
-----------------------------------------------------------------------------
-- | Semi-implicit Euler integration toward the sun; comets that leave the
-- viewport (with margin) or hit the sun are removed.
stepComet :: Double -> Comet -> [Comet]
stepComet dtMs Comet {..}
  | escaped || swallowed = []
  | otherwise =
      [ Comet
        { cometX = cometX + vx' * dt
        , cometY = cometY + vy' * dt
        , cometVX = vx'
        , cometVY = vy'
        , cometHue = cometHue
        }
      ]
  where
    dt = dtMs / 1000
    dx = center - cometX
    dy = center - cometY
    r2 = max 100 (dx * dx + dy * dy)
    r  = sqrt r2
    a  = mu / r2
    vx' = cometVX + a * dx / r * dt
    vy' = cometVY + a * dy / r * dt
    escaped = cometX < -200 || cometX > side + 200
           || cometY < -200 || cometY > side + 200
    swallowed = r < 26
-----------------------------------------------------------------------------
-- | Launch tangentially at roughly orbital speed, so most comets slingshot.
launchComet :: (Double, Double) -> Comet
launchComet (x, y) = Comet x y vx vy huePick
  where
    dx = x - center
    dy = y - center
    r  = max 40 (sqrt (dx * dx + dy * dy))
    v  = sqrt (mu / r) * 0.9
    vx = -dy / r * v
    vy = dx / r * v
    huePick = (round (x + y * 7) :: Int) `mod` 360
-----------------------------------------------------------------------------
viewModel :: () -> () -> Model -> View () Model Action
viewModel _ _ m =
  H.div_
  [ P.class_ "app" ]
  [ H.header_
    [ P.class_ "hero" ]
    [ H.h1_ [] [ "🍜 🪐 ", H.a_ [ P.href_ repoUrl ] [ "miso-canvas2d" ] ]
    , H.p_ [ P.class_ "tagline" ]
      [ "A procedural solar system with gravity-assisted comets, drawn on a "
      , "2D canvas from Haskell compiled to WebAssembly."
      ]
    , H.a_ [ P.class_ "gh", P.href_ repoUrl ] [ "View source on GitHub" ]
    ]
  , H.main_
    [ P.class_ "panel" ]
    [ Canvas.canvas
      [ P.width_ (ms (round side :: Int))
      , P.height_ (ms (round side :: Int))
      , P.class_ "stage"
      , on "click" clickDecoder (\p _ _ -> LaunchComet p)
      ]
      (\_ -> pure ())
      (\() -> drawScene m)
    , H.p_ [ P.class_ "hint" ]
      [ "Click or tap anywhere in space to launch a comet into orbit." ]
    , H.div_
      [ P.class_ "controls" ]
      [ H.button_ [ P.class_ "btn", onClick AddPlanet ] [ "+ planet" ]
      , H.button_ [ P.class_ "btn", onClick RemovePlanet ] [ "− planet" ]
      , H.button_ [ P.class_ "btn", onClick TogglePause ]
        [ text (if m ^. paused then "resume" else "pause") ]
      , H.button_ [ P.class_ "btn", onClick ToggleTrails ]
        [ text (if m ^. trails then "trails: on" else "trails: off") ]
      , H.button_ [ P.class_ "btn", onClick ClearComets ] [ "clear comets" ]
      ]
    ]
  , H.footer_
    [ P.class_ "foot" ]
    [ H.p_ []
      [ "Built with "
      , H.a_ [ P.href_ "https://github.com/dmjio/miso" ] [ "miso" ]
      , ", a Haskell web framework — rendered via "
      , H.code_ [] [ "Miso.Canvas" ]
      , " and "
      , H.code_ [] [ "rAFSub" ]
      , "."
      ]
    ]
  ]
  where
    repoUrl = "https://github.com/haskell-miso/miso-canvas2d"
-----------------------------------------------------------------------------
-- | Decode a click into logical canvas coordinates, scaling CSS pixels by
-- the element's rendered size so it works at any responsive width.
clickDecoder :: Decoder (Double, Double)
clickDecoder = Decoder
  { decodeAt = DecodeTargets [ [], [ "target" ] ]
  , decoder = withArray "click" $ \case
      [ ev, tgt ] -> do
        (ox, oy) <- flip (withObject "event") ev $ \o ->
          (,) <$> o .: "offsetX" <*> o .: "offsetY"
        (cw, ch) <- flip (withObject "target") tgt $ \o ->
          (,) <$> o .: "clientWidth" <*> o .: "clientHeight"
        pure (side * ox / max 1 cw, side * oy / max 1 ch)
      _ -> fail "expected [event, target]"
  }
-----------------------------------------------------------------------------
-- | Planet orbit parameters, derived procedurally from the planet index.
data Orbit = Orbit
  { orbitRadius :: Double
  , orbitSize   :: Double
  , orbitHue    :: Int
  , orbitSpeed  :: Double -- ^ radians per second (Kepler-ish: slower when far)
  , hasRing     :: Bool
  , hasMoon     :: Bool
  }
-----------------------------------------------------------------------------
orbit :: Int -> Orbit
orbit i = Orbit
  { orbitRadius = radius
  , orbitSize   = 5 + fromIntegral ((i * 7) `mod` 9)
  , orbitHue    = (i * 47 + 10) `mod` 360
  , orbitSpeed  = 14 / (radius ** 1.5) * 60
  , hasRing     = i `mod` 3 == 2
  , hasMoon     = i `mod` 2 == 1
  }
  where
    radius = 68 + fromIntegral i * 30
-----------------------------------------------------------------------------
drawScene :: Model -> Canvas ()
drawScene m = do
  -- background: translucent fill leaves motion trails, opaque wipes clean
  Canvas.globalCompositeOperation SourceOver
  Canvas.fillStyle (Canvas.color bg)
  Canvas.fillRect (0, 0, side, side)
  drawSun
  forM_ [ 0 .. (m ^. planets) - 1 ] $ \i ->
    drawPlanet t (orbit i)
  forM_ (m ^. comets) drawComet
  drawHud m
  where
    t = (m ^. time') / 1000
    bg | m ^. trails = CSS.rgba 6 8 20 0.28
       | otherwise   = CSS.rgba 6 8 20 1.0
-----------------------------------------------------------------------------
drawSun :: Canvas ()
drawSun = do
  glow <- Canvas.createRadialGradient (center, center, 4, center, center, 90)
  Canvas.addColorStop (0, CSS.rgba 255 235 160 1) glow
  Canvas.addColorStop (0.25, CSS.rgba 255 180 60 0.9) glow
  Canvas.addColorStop (0.6, CSS.rgba 255 120 30 0.25) glow
  Canvas.addColorStop (1, CSS.rgba 255 120 30 0) glow
  Canvas.fillStyle (Canvas.gradient glow)
  Canvas.beginPath ()
  Canvas.arc (center, center, 90, 0, 2 * pi)
  Canvas.fill ()
-----------------------------------------------------------------------------
drawPlanet :: Double -> Orbit -> Canvas ()
drawPlanet t Orbit {..} = do
  -- faint orbit ring
  Canvas.strokeStyle (Canvas.color (CSS.rgba 140 160 255 0.14))
  Canvas.lineWidth 1
  Canvas.beginPath ()
  Canvas.arc (center, center, orbitRadius, 0, 2 * pi)
  Canvas.stroke ()
  -- planet body
  let angle = t * orbitSpeed
      px = center + orbitRadius * cos angle
      py = center + orbitRadius * sin angle
  Canvas.save ()
  Canvas.translate (px, py)
  when hasRing $ do
    Canvas.strokeStyle (Canvas.color (CSS.hsla orbitHue 70 75 0.7))
    Canvas.lineWidth 2
    Canvas.beginPath ()
    Canvas.save ()
    Canvas.rotate 0.5
    Canvas.scale (1, 0.35)
    Canvas.arc (0, 0, orbitSize * 2, 0, 2 * pi)
    Canvas.restore ()
    Canvas.stroke ()
  Canvas.fillStyle (Canvas.color (CSS.hsl orbitHue 65 55))
  Canvas.beginPath ()
  Canvas.arc (0, 0, orbitSize, 0, 2 * pi)
  Canvas.fill ()
  -- simple day/night shading
  Canvas.fillStyle (Canvas.color (CSS.rgba 0 0 0 0.35))
  Canvas.beginPath ()
  Canvas.arc (orbitSize * 0.4 * cos (angle + pi), orbitSize * 0.4 * sin (angle + pi), orbitSize, 0, 2 * pi)
  Canvas.save ()
  Canvas.clip ()
  Canvas.beginPath ()
  Canvas.arc (0, 0, orbitSize, 0, 2 * pi)
  Canvas.fill ()
  Canvas.restore ()
  when hasMoon $ do
    let ma = t * orbitSpeed * 5
    Canvas.fillStyle (Canvas.color (CSS.rgb 200 200 210))
    Canvas.beginPath ()
    Canvas.arc (orbitSize * 2.2 * cos ma, orbitSize * 2.2 * sin ma, 2, 0, 2 * pi)
    Canvas.fill ()
  Canvas.restore ()
-----------------------------------------------------------------------------
drawComet :: Comet -> Canvas ()
drawComet Comet {..} = do
  let speed = sqrt (cometVX * cometVX + cometVY * cometVY)
      tailLen = min 40 (speed * 0.25)
  Canvas.strokeStyle (Canvas.color (CSS.hsla cometHue 90 70 0.8))
  Canvas.lineWidth 2
  Canvas.beginPath ()
  Canvas.moveTo (cometX, cometY)
  Canvas.lineTo
    ( cometX - cometVX / max 1 speed * tailLen
    , cometY - cometVY / max 1 speed * tailLen
    )
  Canvas.stroke ()
  Canvas.fillStyle (Canvas.color (CSS.hsl cometHue 90 80))
  Canvas.beginPath ()
  Canvas.arc (cometX, cometY, 3, 0, 2 * pi)
  Canvas.fill ()
-----------------------------------------------------------------------------
drawHud :: Model -> Canvas ()
drawHud m = do
  Canvas.fillStyle (Canvas.color (CSS.rgba 232 234 242 0.75))
  Canvas.font "13px monospace"
  Canvas.fillText (stats, 12, side - 14)
  where
    stats = mconcat
      [ ms (round (m ^. fps) :: Int), " fps · "
      , ms (m ^. planets), " planets · "
      , ms (Prelude.length (m ^. comets)), " comets"
      , if m ^. paused then " · paused" else ""
      ]
-----------------------------------------------------------------------------
