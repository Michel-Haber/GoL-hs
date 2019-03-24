module Main where

import           Control.Monad
import           GLib
import           Lib

-- The board size is (x, y)
type Size = Int

-- Standard game rules
stdRules :: Size -> Rules
stdRules s (Coord (x, y), _, _)
  | (x < 0) || (y < 0) || (x >= s) || (y >= s) = CellState False
stdRules _ (_, CellState True, c)
  | (c == 2) || (c == 3) = CellState True
  | otherwise = CellState False
stdRules _ (_, CellState False, 3) = CellState True
stdRules _ _ = CellState False

-- The standard neighbors function
stdNeighbors :: Neighbors
stdNeighbors (Coord (x, y)) =
  [ Coord (a, b)
  | a <- [x - 1, x, x + 1]
  , b <- [y - 1, y, y + 1]
  , (a /= x) || (b /= y)
  ]

-- Main function
main :: IO ()
main = do
  putStrLn "Choose board size S (10, 5, 3, etc...)"
  input <- getLine
  putStrLn "Choose starting points(e.g: (1,1) (2,2) (3,3))"
  start <- getLine
  let size = read input
      rules = stdRules size
      initial = map (Coord . read) . words $ start
      game = advance rules stdNeighbors
  runGraphics size game initial
