-- chaosbox adaptation of: https://www.kovach.me/posts/2018-03-07-generating-art.html
module Main where

import           ChaosBox
import           Control.Monad            (replicateM)
import           Control.Monad.Random     (getRandomR, uniform, weighted)
import           Control.Monad.Reader     (asks)
import           Data.Foldable            (for_)
import           Data.List                (nub)
import           Graphics.Rendering.Cairo
import           Linear.V2
import           Linear.Vector
import qualified Numeric.Noise.Perlin     as P

--  a---d
--  |   |
--  b---c
data Quad = Quad
  { quadA :: V2 Double
  , quadB :: V2 Double
  , quadC :: V2 Double
  , quadD :: V2 Double
  } deriving (Eq, Ord)

fromIntegralVector :: V2 Int -> V2 Double
fromIntegralVector (V2 x y) = V2 (fromIntegral x) (fromIntegral y)

genQuadGrid :: Generate [Quad]
genQuadGrid = do
  (w, h)  <- getSize @Int
  vectors <- replicateM 800 $ do
    v <- V2 <$> getRandomR (3, w `div` 2 - 3) <*> getRandomR (3, h `div` 2 - 3)
    pure $ v ^* 2
  pure . nub . flip map vectors $ \v ->
    let v' = fromIntegralVector v
    in  Quad v' (v' ^+^ V2 0 1.5) (v' ^+^ V2 1.5 1.5) (v' ^+^ V2 1.5 0)

renderClosedPath :: [V2 Double] -> Render ()
renderClosedPath (V2 x y:vs) = do
  newPath
  moveTo x y
  for_ vs $ \v -> let V2 x' y' = v in lineTo x' y'
  closePath
renderClosedPath [] = pure ()

renderQuad :: Quad -> Render ()
renderQuad Quad {..} = renderClosedPath [quadA, quadB, quadC, quadD]

quadAddNoise :: Quad -> Generate Quad
quadAddNoise Quad {..} = do
  perlinSeed <- fromIntegral <$> asks gcSeed

  let
    perlinOctaves     = 5
    perlinScale       = 0.1
    perlinPersistance = 0.5
    perlinNoise =
      P.perlin (round perlinSeed) perlinOctaves perlinScale perlinPersistance
    perlin2d (V2 x y) =
      P.noiseValue perlinNoise (x + perlinSeed, y + perlinSeed, perlinSeed)
        - 0.5
    addNoise v = let noise = perlin2d v in v ^+^ V2 (noise / 5) (noise / 8)

  pure
    $ Quad (addNoise quadA) (addNoise quadB) (addNoise quadC) (addNoise quadD)

renderSketch :: Generate ()
renderSketch = do
  let eggshell = HSV 71 0.13 0.96
      darkGunmetal = HSV 170 0.30 0.16
      teaGreen = HSV 81 0.25 0.94
      vividTangerine = HSV 11 0.40 0.92
      englishVermillion = HSV 355 0.68 0.84

  fillScreenHSV eggshell

  cairo $ setLineWidth 0.15

  quads      <- genQuadGrid
  noisyQuads <- traverse quadAddNoise quads

  for_ noisyQuads $ \quad -> do
    strokeOrFill <- weighted [(fill, 0.4), (stroke, 0.6)]
    color <- uniform [teaGreen, vividTangerine, englishVermillion, darkGunmetal]
    cairo $ do
      renderQuad quad
      setSourceHSV color *> strokeOrFill

main :: IO ()
main = runChaosBoxIOWith
  ( \opts -> opts { optWidth  = 60
                  , optHeight = 60
                  , optScale  = 20
                  , optSeed   = Just 1520476193207
                  }
  )
  renderSketch
