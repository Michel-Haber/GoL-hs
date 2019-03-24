module GLib
  ( runGraphics
  ) where

import Lib
import Graphics.Gloss

type Size = Int

fint = fromIntegral

windowSize :: Int
windowSize = 600

window :: Display
window = InWindow "Game Of Life" (windowSize, windowSize) (10, 10)

fps :: Int
fps = 30

background :: Color
background = black

cellColor :: Color
cellColor = white

cellTemplate :: Float -> Picture
cellTemplate s = color cellColor $ rectangleSolid s s

render :: Size -> Board -> Picture
render s = pictures . map (renderCell s . getCoords)

renderCell :: Size -> (Int, Int) -> Picture
renderCell s c = uncurry translate pixelCoords $ cellTemplate cellSize
  where
    cellSize = fint windowSize / fint s
    pixelCoords = translateCoords s c

translateCoords :: Size -> (Int, Int) -> (Float, Float)
translateCoords s (x,y) = (translate s x, translate s y)
  where
    translate n k = fint k / fint n * fint windowSize
                  - fint windowSize / 2 + 5


-- Board size, Stepper function
runGraphics :: Size -> (Board -> Board) -> Board -> IO ()
runGraphics size evolve board =
  simulate window background fps board (render size) (\_ _ b -> evolve b)
