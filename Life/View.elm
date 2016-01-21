module Life.View where

import Array
import Color
import Graphics.Collage exposing (collage, filled, Form, move, rect)
import Graphics.Element
import Html exposing (..)
import Html.Attributes exposing (max, min, style, value)
import Html.Events exposing (onClick)

import Life.Grid exposing (Grid, to2d)
import Life.Input exposing (..)
import Life.Model exposing (Model)

-- Draw a single cell in
renderCell : Int -> Int -> Int -> Bool -> Graphics.Collage.Form
renderCell num_cols cell_size index alive =
  let
    color = if alive then Color.rgb 255 0 0 else Color.rgb 0 0 0
    (row, col) = to2d num_cols index -- TODO: This use of to2d is a little skeezy.
    toX = toFloat (col * cell_size + cell_size // 2)
    toY = toFloat (row * cell_size + cell_size // 2)
  in
    rect (toFloat cell_size) (toFloat cell_size) |> filled color |> move (toX, toY)

-- Draw the full grid of cells into a collage
renderGrid : Grid -> Int -> Int -> (Int -> Bool -> Graphics.Collage.Form) -> Graphics.Element.Element
renderGrid grid width height cell_renderer =
  let
    shift_x = toFloat (-1 * width // 2)
    shift_y = toFloat (-1 * height // 2)
    cells = Array.indexedMap cell_renderer grid.cells
            |> Array.map (\c -> move (shift_x, shift_y) c)
            |> Array.toList
    background = rect (toFloat width) (toFloat height) |> filled (Color.rgb 0 0 0)
  in
    collage
      width
      height
      (background :: cells)

countStyle : Attribute
countStyle =
  style
    [ ("font-size", "20px")
    , ("font-family", "monospace")
    , ("display", "inline-block")
    , ("width", "50px")
    , ("text-align", "center")
    ]

view : Signal.Address Input -> Model -> Html
view address model =
  let
    cell_renderer = renderCell model.grid.num_cols model.cell_size
    grid_size = model.cell_size * model.grid.num_cols
    elem = renderGrid model.grid grid_size grid_size cell_renderer
  in
    div []
          [ button [ onClick address Reset ] [ text "reset" ]
          , text "cell size"
          , button [ onClick address (ResizeCells (model.cell_size - 1)) ] [ text "-" ]
          , div [ countStyle ] [ text (toString model.cell_size) ]
          , button [ onClick address (ResizeCells (model.cell_size + 1)) ] [ text "+" ]
          , meter [ Html.Attributes.min "0"
                  , Html.Attributes.max "999999"
                  , value (toString model.seed)]
              []
          , fromElement elem ]
