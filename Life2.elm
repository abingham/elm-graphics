module Life2 where

import Array
import Color
import Graphics.Collage exposing (collage, filled, Form, move, rect)
import Graphics.Element
import Html
import Html.Events
import List exposing (filter, length, map)
import Maybe
import Random
import Time

-- TODO: Use time for random seed, or let use specify it
-- TODO: Controls for grid size
-- TODO: Let user bring cells to life via clicking or something.
-- TODO: Control over cell size
-- TODO: Make it faster! Surely we can improve on this...

-- Here's how to use StartApp like a boss...
-- http://package.elm-lang.org/packages/evancz/start-app/2.0.2/StartApp

main : Signal Html.Html
main =
  -- Signal.map view model
  Signal.map buildHtml viewSignal

--
-- Model stuff
--

type Action = None | Reset

type alias Input =
  { action : Action
  , tick : Int
  }

htmlMailbox : Signal.Mailbox Action
htmlMailbox = Signal.mailbox None

input : Signal Input
input = Signal.map2 Input htmlMailbox.signal (ticks 10)

model : Signal Model
model =
  -- TODO: Route more signals through the update mechanism, and let it update
  -- the model appropriately. Clever... For example, we need an action to update
  -- the seed and restart the simulation.
  Signal.foldp update (createModel 200 200 12345) input

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

update : Input -> Model -> Model
update input model =
  case input.action of
    None ->
      { model |
          cells = Array.initialize (model.num_rows * model.num_cols) (newVal model)
      }

    Reset ->
      createModel 200 200 12345

--
-- View stuff
--

cell_size : Int
cell_size = 5

renderCell : Int -> Int -> Bool -> Graphics.Collage.Form
renderCell num_cols index alive =
  let
    color = if alive then Color.rgb 255 0 0 else Color.rgb 0 0 0
    (row, col) = to2d num_cols index
    toX = toFloat (col * cell_size)
    toY = toFloat (row * cell_size)
  in
    rect (toFloat cell_size) (toFloat cell_size) |> filled color |> move (toX, toY)

view : Model -> Graphics.Element.Element
view model =
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

viewSignal : Signal Graphics.Element.Element
viewSignal = Signal.map view model

buildHtml : Graphics.Element.Element -> Html.Html
buildHtml elem =
  Html.div []
      [ Html.text "hola!"
      , Html.button [ Html.Events.onClick htmlMailbox.address Reset ] [ Html.text "reset" ]
      , Html.fromElement elem ]

--
-- Utility stuff
--

randomBools : Int -> Random.Seed -> List Bool
randomBools size seed =
  let
    gen = Random.list size Random.bool
    (vals, seed) = Random.generate gen seed
  in
    vals

ticks : Int -> Signal Int
ticks fps = (Signal.foldp (\tick total -> total + 1) 0 (Time.fps fps))
