module Lib
  ( Board
  , Rules
  , Neighbors
  , Coord(Coord)
  , CellState(CellState)
  , getCoords
  , advance
  ) where

-- Game of life Haskell Implementation
import           Control.Monad.State
import           Data.List
import qualified Data.Map            as M

-- The cell state is isomorphic too Bool.
newtype CellState = CellState
  { getState :: Bool
  }

-- The coordinate of a cell
newtype Coord = Coord
  { getCoords :: (Int, Int)
  } deriving (Eq, Ord, Show, Read)

-- The state of the board is simply the coordinates of its live cells
type Board = [Coord]

-- The state carried in the State Monad, used to count tags for cells
type TallyState = State (M.Map Coord (CellState, Int)) ()

-- The type of the game rules
type Rules = (Coord, CellState, Int) -> CellState

-- The type for the neighbor functions
type Neighbors = Coord -> [Coord]

-- Tally the neighbors of live cells and relevant dead cells
tallyBoard :: Neighbors -> Board -> TallyState
tallyBoard = mapM_ . tallyCoord

-- Tally a live cell: Set its state to True (alive) and tag its neighbors
-- This function takes the neighbors function as its first argument. We can use
-- different neighbor functions to change the zone of influence of a cell
tallyCoord :: Neighbors -> Coord -> TallyState
tallyCoord nb c = do
  let merge (CellState a1, b1) (CellState a2, b2) =
        (CellState $ a1 || a2, b1 + b2)
  s <- get
  let s' = M.insertWith merge c (CellState True, 0) s
  let n = nb c
  put $ foldl' (\acc x -> M.insertWith merge x (CellState False, 1) acc) s' n

-- Extract the results from a TallyState
toResults :: TallyState -> [(Coord, CellState, Int)]
toResults = map flatten . M.toList . flip execState M.empty
  where
    flatten (x, (y, z)) = (x, y, z)

-- Use A Rules and Neighbors function to advance the board one step in time
advance :: Rules -> Neighbors -> Board -> Board
advance rules nb =
  map first . filter (getState . rules) . toResults . tallyBoard nb
  where
    first (x, _, _) = x
