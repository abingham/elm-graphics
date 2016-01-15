import Color exposing (rgb)
import Html
import Graphics.Element exposing (..)
import Graphics.Collage
import Graphics.Collage exposing (Form, move, circle, filled)
import Mouse
import StartApp.Simple as StartApp
import Time exposing (every, second, Time)

main =
  StartApp.start { model = model, view = view, update = update }

type alias Ball =
  { x : Int
  , y : Int
  , vx : Float
  , vy : Float
  {- size, color, etc.-}
  }

createBall : Int -> Int -> Float -> Float -> Ball
createBall x y vx vy = { x = x, y = y, vx = vx, vy = vy }

type alias Model = List Ball

model : Model
model = [createBall 10 10 0 1]

update : Signal Time -> Model -> Model
update action model = model -- todo

view address model = Html.div [] [Html.text "TODO!"]



-- Eventually we want to render this on the screen and the
-- function to do this requires a List Form not just a single
-- Form. So we write a function which returns a Singleton list
-- and apply it to each value in our formSignal.
formListSignal : Signal Form -> Signal (List Form)
formListSignal sf = Signal.map (\n -> [n]) sf

field =
  { size_x = 400, size_y = 400 }

ticks : Signal Int
ticks = (Signal.foldp (\tick total -> total + 1) 0 (every (0.01 * second)))

boundedY y =
  if y < 0 then
    boundedY (y + field.size_y)
  else if y > field.size_y then
    boundedY (y - field.size_y)
  else
    y

circleAtTime : Int -> Form
circleAtTime t =
  move (0, boundedY t) (filled (rgb 100 100 100) (circle 20))

circleSignal : Signal Form
circleSignal = Signal.map circleAtTime ticks

-- Finally, we must turn that into a Signal Element to render
-- on the screen. We partially apply Graphics.Collage.collage
-- to return an element of size 400x400 and apply it to the
-- values of formListSignal by using Signal.map again
elementSignal : Signal Form -> Signal Element
elementSignal forms =
  let {size_x, size_y} = field
  in Signal.map
     (Graphics.Collage.collage size_x size_y)
     (formListSignal forms)

-- Finally we hand this off to main and it renders

buildHtml : Element -> Html.Html
buildHtml elem =
  Html.div []
        [ Html.text "hola!"
        , Html.fromElement elem]

-- main =
--   Signal.map buildHtml (elementSignal circleSignal)
