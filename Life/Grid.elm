module Life.Grid where

import Array
import List exposing (filter, length, map)
import Random

import Life.Util exposing (randomBools)

type alias Grid =
  { cells : Array.Array Bool
  , num_rows : Int
  , num_cols : Int
  , seed : Int
  }

get : Grid -> Int -> Bool
get grid index = Array.get index grid.cells |> Maybe.withDefault False

create : Int -> Int -> Int -> Grid
create num_rows num_cols seed =
  let
    full_size = (num_rows * num_cols)
  in
    { cells = randomBools full_size (Random.initialSeed seed) |> Array.fromList
    , num_rows = num_rows
    , num_cols = num_cols
    , seed = seed
    }

bound : Int -> Int -> (Int, Int) -> (Int, Int)
bound num_rows num_cols (row, col) = (row % num_rows, col % num_cols)

toFlat : Int -> (Int, Int) -> Int
toFlat num_cols (row, col) = row * num_cols + col

to2d : Int -> Int -> (Int, Int)
to2d num_cols index = (index // num_cols, index % num_cols)

neighborCoords : Int -> Int -> Int -> List Int
neighborCoords index num_cols num_rows =
  let
    (row, col) = to2d num_cols index
  in
    [ (row - 1, col - 1)
    , (row - 1, col)
    , (row - 1, col + 1)
    , (row, col - 1)
    , (row, col + 1)
    , (row + 1, col - 1)
    , (row + 1, col)
    , (row + 1, col + 1)]
    |> map (bound num_rows num_cols)
    |> map (toFlat num_cols)

stepCell : Grid -> Int -> Bool
stepCell grid index =
  let
    indices = (neighborCoords index grid.num_cols grid.num_rows)
    neighborVals = map (get grid) indices
    livingNeighbors = filter identity neighborVals |> length
  in
    if livingNeighbors > 3 then
      False
    else if livingNeighbors < 2 then
      False
    else if livingNeighbors == 3 then
      True
    else
      get grid index

step : Grid -> Grid
step grid =
  { grid |
      cells = Array.initialize (grid.num_rows * grid.num_cols) (stepCell grid)
  }
