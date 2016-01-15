module Life where

import Html
import StartApp.Simple as StartApp
import Time

main =
  StartApp.start { model = model, view = view, update = update }

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

createModel : Int -> Int -> Model
createModel size_x size_y =
  { cells = List.repeat size_y (List.repeat size_x False)
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
      liveness = List.map (\(x, y) -> value m x y) coords
      alive = List.filter identity liveness
      neighbors = List.length alive
  in
    if neighbors >= 3 then
      False
    else if neighbors <= 1 then
      False
    else
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


view address model = Html.text "TODO!"
