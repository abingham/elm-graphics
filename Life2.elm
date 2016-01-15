module Life2 where

import Array
import Graphics.Element
import Html
import List exposing (filter, length, map)
import Maybe
import Random
import StartApp.Simple as StartApp
import Time

main =
  StartApp.start { model = createModel 50 50, view = view, update = update }


--
-- Model stuff
--

type alias Model =
  { cells : Array.Array Bool
  , num_rows : Int
  , num_cols : Int
  }

createModel : Int -> Int -> Model
createModel num_rows num_cols =
  let
    full_size = (num_rows * num_cols)
  in
    { cells = randomBools full_size |> Array.fromList
    , num_rows = num_rows
    , num_cols = num_cols
    }

-- Get the value at (row, col) in the model cells. This does wrapping in both
-- X and Y, so feel free to pass in coordinates that are out of range.
get : Int -> Int -> Model -> Bool
get row col model =
  let
    row = row % model.num_rows
    col = col % model.num_cols
    index = (row * model.num_cols + col)
  in
    Array.get index model.cells |> Maybe.withDefault False


--
-- Update stuff
--

neighborCoords : Int -> Int -> List (Int, Int)
neighborCoords row col =
  [ (row - 1, col - 1)
  , (row - 1, col)
  , (row - 1, col + 1)
  , (row, col - 1)
  , (row, col + 1)
  , (row + 1, col - 1)
  , (row + 1, col)
  , (row + 1, col + 1)
  ]

-- Get the next round's value for a flat index
newVal : Model -> Int -> Bool
newVal model index =
  let
    row = index // model.num_cols
    col = index % model.num_cols
    neighborVals = map (\(r, c) -> get r c model) (neighborCoords row col)
    livingNeighbors = filter identity neighborVals |> length
  in
    if livingNeighbors > 3 then
      False
    else if livingNeighbors < 2 then
      False
    else if livingNeighbors == 3 then
      True
    else
      get row col model

update tick model =
  { model |
      cells = Array.initialize (model.num_rows * model.num_cols) (newVal model)
  }


--
-- View stuff
--

view address model = Graphics.Element.show model |> Html.fromElement


--
-- Utility stuff
--

randomBools : Int -> List Bool
randomBools size =
  let
    gen = Random.list size Random.bool
    (vals, seed) = Random.generate gen (Random.initialSeed 1234)
  in
    vals

ticks : Float -> Signal Int
ticks rate = (Signal.foldp (\tick total -> total + 1) 0 (Time.every (rate * Time.second)))
