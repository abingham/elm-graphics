module Life.App where

import Array
import Color
import Effects exposing (Effects)
import Graphics.Collage exposing (collage, filled, Form, move, rect)
import Graphics.Element
import Html
import Html.Events
import List exposing (filter, length, map)
import Maybe
import Random
import StartApp

import Life.Util exposing (noFx, randomBools, ticks)

-- TODO: Use time for random seed, or let use specify it
-- TODO: Controls for grid size
-- TODO: Let user bring cells to life via clicking or something.
-- TODO: Control over cell size
-- TODO: Make it faster! Surely we can improve on this...

-- Aggregate of input types
type Input = Tick Int | Reset

app : StartApp.App Model
app =
  StartApp.start
    { init = noFx (createModel 200 200 12345)
    , view = view
    , update = update
    , inputs = [ Signal.map (\t -> Tick t) (ticks 10)] }

main : Signal Html.Html
main =
  app.html

--
-- Model stuff
--

type alias Model =
  { cells : Array.Array Bool
  , num_rows : Int
  , num_cols : Int
  , seed : Int
  }

get : Model -> Int -> Bool
get model index = Array.get index model.cells |> Maybe.withDefault False

createModel : Int -> Int -> Int -> Model
createModel num_rows num_cols seed =
  let
    full_size = (num_rows * num_cols)
  in
    { cells = randomBools full_size (Random.initialSeed seed) |> Array.fromList
    , num_rows = num_rows
    , num_cols = num_cols
    , seed = seed
    }

--
-- Update stuff
--

-- convert flat index to (row, col) tuple
to2d : Int -> Int -> (Int, Int)
to2d num_cols index = (index // num_cols, index % num_cols)

-- Convert a (row, col) tuple to a flat index
toFlat : Int -> (Int, Int) -> Int
toFlat num_cols (row, col) = row * num_cols + col

bound : Int -> Int -> (Int, Int) -> (Int, Int)
bound num_rows num_cols (row, col) = (row % num_rows, col % num_cols)

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

update : Input -> Model -> (Model, Effects Input)
update input model =
  let
    m =
      case input of
        Tick t ->
          { model |
              cells = Array.initialize (model.num_rows * model.num_cols) (newVal model)
          }

        Reset ->
          createModel 200 200 12345
  in
    noFx m

--
-- View stuff
--

cell_size : Int
cell_size = 5

-- Draw a single cell in its correct final position
renderCell : Int -> Int -> Bool -> Graphics.Collage.Form
renderCell num_cols index alive =
  let
    color = if alive then Color.rgb 255 0 0 else Color.rgb 0 0 0
    (row, col) = to2d num_cols index
    toX = toFloat (col * cell_size)
    toY = toFloat (row * cell_size)
  in
    rect (toFloat cell_size) (toFloat cell_size) |> filled color |> move (toX, toY)

-- Draw the full grid of cells into a collage
renderModel : Model -> Graphics.Element.Element
renderModel model =
  let
    render = (renderCell model.num_cols)
    yshift = toFloat (-1 * model.num_cols * cell_size // 2)
    xshift = toFloat (-1 * model.num_rows * cell_size // 2)
    cells = Array.indexedMap render model.cells |> Array.map (move (xshift, yshift)) |> Array.toList
  in
    collage
      (model.num_cols * cell_size * 2)
      (model.num_rows * cell_size * 2)
      cells

view : Signal.Address Input -> Model -> Html.Html
view address model =
  let
    elem = renderModel model
  in
    Html.div []
          [ Html.text "hola!"
          , Html.button [ Html.Events.onClick address Reset ] [ Html.text "reset" ]
          , Html.fromElement elem ]
