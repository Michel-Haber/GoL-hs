module GLib
  ( runGraphics
  ) where

import           Graphics.Gloss
import           Lib

-- Size of the board
type Size = Int

-- Main graphic simulation function
-- Takes the Board size, the stepper function, and the initial board
runGraphics :: Size -> (Board -> Board) -> Board -> IO ()
runGraphics size evolve board =
  simulate window background fps board (render size) (\_ _ b -> evolve b)

-- fromIntegral is too long, fint is better :p
fint = fromIntegral

-- Configuration
windowSize = 600

fps = 1

background = black

cellColor = white

-- The window running the animation
window :: Display
window = InWindow "Game Of Life" (windowSize, windowSize) (10, 10)

-- Given the cell size, gives back a Cell Picture
cellTemplate :: Float -> Picture
cellTemplate s = color cellColor $ rectangleSolid s s

-- Given the board size, renders a whole board into a Picture
render :: Size -> Board -> Picture
render s = rotate 90 . pictures . map (renderCell s . getCoords)

-- Given the board size, renders a cell into a Picture
renderCell :: Size -> (Int, Int) -> Picture
renderCell s c = uncurry translate pixelCoords $ cellTemplate cellSize
  where
    cellSize = fint windowSize / fint s
    pixelCoords = translateCoords s c

-- Translate board coordinates into graphics coordinated
translateCoords :: Size -> (Int, Int) -> (Float, Float)
translateCoords s (x, y) = (translate s x, translate s y)
  where
    translate n k =
      fint k / fint n * fint windowSize - fint windowSize / 2 + cellSize / 2
    cellSize = fint windowSize / fint s
