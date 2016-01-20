module Life.View where

import Array
import Color
import Graphics.Collage exposing (collage, filled, Form, move, rect)
import Graphics.Element
import Html
import Html.Events

import Life.Grid exposing (Grid, to2d)
import Life.Input exposing (..)
import Life.Model exposing (Model)

-- Draw a single cell in its correct final position
renderCell : Int -> Int -> Int -> Bool -> Graphics.Collage.Form
renderCell num_cols cell_size index alive =
  let
    color = if alive then Color.rgb 255 0 0 else Color.rgb 0 0 0
    (row, col) = to2d num_cols index -- TODO: This is a little skeezy.
    toX = toFloat (col * cell_size)
    toY = toFloat (row * cell_size)
  in
    rect (toFloat cell_size) (toFloat cell_size) |> filled color |> move (toX, toY)

-- Draw the full grid of cells into a collage
renderGrid : Grid -> Int -> Int -> (Int -> Bool -> Graphics.Collage.Form) -> Graphics.Element.Element
renderGrid grid width height cell_renderer =
  let
    cells = Array.indexedMap cell_renderer grid.cells
          |> Array.toList
  in
    collage
      width
      height
      cells

view : Signal.Address Input -> Model -> Html.Html
view address model =
  let
    cell_renderer = renderCell model.grid.num_cols model.cell_size
    grid_size = model.cell_size * model.grid.num_cols
    elem = renderGrid model.grid grid_size grid_size cell_renderer
  in
    Html.div []
          [ Html.text "hola!"
          , Html.button [ Html.Events.onClick address Reset ] [ Html.text "reset" ]
          , Html.fromElement elem ]