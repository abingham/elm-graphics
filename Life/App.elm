module Life.App where

import Array
import Color
import Effects exposing (Effects)
import Graphics.Collage exposing (collage, filled, Form, move, rect)
import Graphics.Element
import Html
import Html.Events
import StartApp

import Life.Grid
import Life.Grid exposing (Grid)
import Life.Util exposing (noFx, randomBools, ticks)

-- TODO: Use time for random seed, or let use specify it
-- TODO: Controls for grid size
-- TODO: Let user bring cells to life via clicking or something.
-- TODO: Control over cell size
-- TODO: Make it faster! Surely we can improve on this...

-- Aggregate of input types
type Input = Tick Int | Reset

app : StartApp.App Grid
app =
  StartApp.start
    { init = noFx (Life.Grid.create 200 200 12345)
    , view = view
    , update = update
    , inputs = [ Signal.map (\t -> Tick t) (ticks 10)] }



--
-- Update stuff
--

update : Input -> Grid -> (Grid, Effects Input)
update input model =
  let
    m =
      case input of
        Tick t ->
          Life.Grid.step model

        Reset ->
          Life.Grid.create 200 200 12345
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
    (row, col) = Life.Grid.to2d num_cols index -- TODO: This is a little skeezy.
    toX = toFloat (col * cell_size)
    toY = toFloat (row * cell_size)
  in
    rect (toFloat cell_size) (toFloat cell_size) |> filled color |> move (toX, toY)

-- Draw the full grid of cells into a collage
renderGrid : Grid -> Graphics.Element.Element
renderGrid model =
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

view : Signal.Address Input -> Grid -> Html.Html
view address model =
  let
    elem = renderGrid model
  in
    Html.div []
          [ Html.text "hola!"
          , Html.button [ Html.Events.onClick address Reset ] [ Html.text "reset" ]
          , Html.fromElement elem ]
