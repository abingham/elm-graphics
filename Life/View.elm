module Life.View where

import Array
import Html exposing (..)
import Html.Attributes exposing (max, min, style, type', value)
import Html.Events exposing (on, onClick, targetValue)
import Result exposing (withDefault)
import String exposing (fromList, toInt)

import Life.Grid exposing (Grid, to2d)
import Life.Input exposing (..)
import Life.Model exposing (Model)
import Life.Util exposing (chunks)

-- Draw a single cell in
-- renderCell : Int -> Int -> Int -> Bool -> Graphics.Collage.Form
-- renderCell num_cols cell_size index alive =
--   let
--     color = if alive then Color.rgb 255 0 0 else Color.rgb 0 0 0
--     (row, col) = to2d num_cols index -- TODO: This use of to2d is a little skeezy.
--     toX = toFloat (col * cell_size + cell_size // 2)
--     toY = toFloat (row * cell_size + cell_size // 2)
--   in
--     rect (toFloat cell_size) (toFloat cell_size) |> filled color |> move (toX, toY)

renderCell : Bool -> Char
renderCell alive =
  if alive then
    '#'
  else
    ' '


-- Draw the full grid of cells into a collage
renderGrid : Grid -> Int -> Int -> (Bool -> Char) -> Html
renderGrid grid width height cell_renderer =
  let
    renderings = Array.map renderCell grid.cells |> Array.toList
    lines = chunks (grid.num_cols) renderings |> List.map (String.fromList >> text)
    separated = List.intersperse (br [] []) lines
  in
    pre [gridStyle] separated

countStyle : Attribute
countStyle =
  style
    [ ("font-size", "20px")
    , ("font-family", "monospace")
    , ("display", "inline-block")
    , ("width", "50px")
    , ("text-align", "center")
    ]

gridStyle : Attribute
gridStyle =
  style
    [ ("font-size", "5px")
    , ("font-family", "monospace")
    ]

seedSelector : Int -> Signal.Address Input -> Html
seedSelector seed address =
  input [ type' "number"
        , Html.Attributes.min "0"
        , Html.Attributes.max "999999"
        , value (toString seed)
        , on "click"
             targetValue
            (\str -> toInt str |>
               withDefault seed
               >> SetSeed
               >> Signal.message address)
        ]
    []

view : Signal.Address Input -> Model -> Html
view address model =
  let
    cell_renderer = renderCell
    grid_size = model.cell_size * model.grid.num_cols
    elem = renderGrid model.grid grid_size grid_size cell_renderer
  in
    div []
          [ button [ onClick address Reset ] [ text "reset" ]
          , text "cell size"
          , button [ onClick address (ResizeCells (model.cell_size - 1)) ] [ text "-" ]
          , div [ countStyle ] [ text (toString model.cell_size) ]
          , button [ onClick address (ResizeCells (model.cell_size + 1)) ] [ text "+" ]
          , seedSelector model.seed address
          , elem ]
