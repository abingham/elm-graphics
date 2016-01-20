module Life.View where

import Array
import Color
import Graphics.Collage exposing (collage, filled, Form, move, rect)
import Graphics.Element
import Html
import Html.Events

import Life.Grid exposing (Grid, to2d)
import Life.Input exposing (..)

cell_size : Int
cell_size = 5

-- Draw a single cell in its correct final position
renderCell : Int -> Int -> Bool -> Graphics.Collage.Form
renderCell num_cols index alive =
  let
    color = if alive then Color.rgb 255 0 0 else Color.rgb 0 0 0
    (row, col) = to2d num_cols index -- TODO: This is a little skeezy.
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
