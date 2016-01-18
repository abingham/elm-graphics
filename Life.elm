module Life where

import Color exposing (rgb)
import Graphics.Collage exposing (collage, filled, Form, move, rect)
import Graphics.Element exposing (Element)
import Html
import Random
import StartApp.Simple as StartApp
import Time

main =
  Signal.map display state

displayCell : Int -> Int -> Bool -> Form
displayCell y x alive =
  let
    color = if alive then (rgb 255 0 0) else (rgb 0 0 0)
  in
    (rect 5 5) |> (filled color) |> (move (toFloat (x * 5), toFloat (y * 5)))

displayRow : Int -> List Bool -> List Form
displayRow y row = List.indexedMap (displayCell y) row

display : Model -> Element
display m =
  let
    {cells} = m
    formLists = List.indexedMap displayRow cells
  in
    collage
      (m.size_x * 5)
      (m.size_y * 5)
      (List.concat formLists)

state : Signal Model
state = Signal.foldp update (createModel 200 200) (ticks 1)

-- emit an incremented number every RATE seconds.
ticks : Float -> Signal Int
ticks rate = (Signal.foldp (\tick total -> total + 1) 0 (Time.every (rate * Time.second)))

type alias Model =
  { cells : List (List Bool)
  , size_x : Int
  , size_y : Int
  }

get : Int -> List a -> Maybe a
get n xs = List.drop n xs |> List.head

range : Int -> List Int
range x = [0..(x - 1)]

-- generate (list 10 bool (initialSeed 1234)

randomRow : Int -> Random.Seed -> (List Bool, Random.Seed)
randomRow size seed =
  let
    gen = Random.list size Random.bool
  in
    Random.generate gen seed

randomCells : Int -> Int -> Random.Seed -> List (List Bool)
randomCells size_x size_y seed =
  if size_y < 1 then
    []
  else
    let
      (row, seed) = randomRow size_x seed
    in
      [row] ++ randomCells size_x (size_y - 1) seed

createModel : Int -> Int -> Model
createModel size_x size_y =
  { cells = randomCells size_x size_y (Random.initialSeed 1234)
  , size_x = size_x
  , size_y = size_y
  }

value : Model -> Int -> Int -> Bool
value m x y =
  let
    { size_x, size_y, cells } = m
    x = x % size_x
    y = y % size_y
    xs = get y cells |> Maybe.withDefault []
  in
    get x xs |> Maybe.withDefault False


nextValue : Model -> Int -> Int -> Bool
nextValue m x y =
  let coords = [ (x - 1, y - 1)
               , (x,     y - 1)
               , (x + 1, y - 1)
               , (x - 1, y)
               , (x + 1, y)
               , (x - 1, y + 1)
               , (x,     y + 1)
               , (x + 1, y + 1)
               ]
      alive = value
      liveness = List.map (\(x, y) -> value m x y) coords
      living_neighbors = List.filter identity liveness
      n = List.length living_neighbors
  in
    if n > 3 then
      False
    else if n < 2 then
      False
    else if n = 3 then
      True



model : Model
model = createModel 200 200

updateRow : Model -> Int -> List Bool
updateRow m y = List.map (\x -> nextValue m x y) (range m.size_x)

update : Int -> Model -> Model
update tick model =
  { model |
      cells = List.map (updateRow model) (range model.size_y)
  }
