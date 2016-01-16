module Life2 where

import Array
import Color
import Graphics.Collage exposing (collage, filled, Form, move, rect)
import Graphics.Element
import Html
import List exposing (filter, length, map)
import Maybe
import Random
import StartApp.Simple as StartApp
import Time

main =
  Signal.map view model

--
-- Model stuff
--

model : Signal Model
model =
  Signal.foldp update (createModel 200 200) (ticks 1)

type alias Model =
  { cells : Array.Array Bool
  , num_rows : Int
  , num_cols : Int
  }

get : Model -> Int -> Bool
get model index = Array.get index model.cells |> Maybe.withDefault False

createModel : Int -> Int -> Model
createModel num_rows num_cols =
  let
    full_size = (num_rows * num_cols)
  in
    { cells = randomBools full_size |> Array.fromList
    , num_rows = num_rows
    , num_cols = num_cols
    }

--
-- Update stuff
--

neighborCoords : Int -> Int -> Int -> List Int
neighborCoords index num_cols num_rows =
  let
    up_row = index - num_cols
    down_row = index + num_cols
    full_size = num_cols * num_rows
    bound = (\i -> i % full_size)
  in
    map bound [ up_row - 1
              , up_row
              , up_row + 1
              , index - 1
              , index + 1
              , down_row - 1
              , down_row
              , down_row + 1
              ]

-- Get the next round's value for a flat index
newVal : Model -> Int -> Bool
newVal model index =
  let
    indices = (neighborCoords index model.num_cols model.num_rows)
    neighborVals = map (get model) indices
    livingNeighbors = filter identity neighborVals |> length
  in
    if livingNeighbors > 3 then
      False
    else if livingNeighbors < 2 then
      False
    else if livingNeighbors == 3 then
      True
    else
      get model index

update tick model =
  { model |
      cells = Array.initialize (model.num_rows * model.num_cols) (newVal model)
  }


--
-- View stuff
--

cell_size = 5

renderCell : Int -> Int -> Bool -> Graphics.Collage.Form
renderCell num_cols index alive =
  let
    color = if alive then Color.rgb 255 0 0 else Color.rgb 0 0 0
    row = index // num_cols
    col = index % num_cols
    toX = toFloat (col * cell_size)
    toY = toFloat (row * cell_size)
  in
    rect cell_size cell_size |> filled color |> move (toX, toY)

view : Model -> Graphics.Element.Element
view model =
  let
    render = renderCell model.num_cols
    cells = Array.indexedMap render model.cells |> Array.toList
  in
    collage
      (model.num_cols * cell_size)
      (model.num_rows * cell_size)
      cells


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
ticks rate = (Signal.foldp (\tick total -> total + 1) 0 (Time.every (rate * Time.millisecond)))
