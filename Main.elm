import Color exposing (rgb)
import Graphics.Element exposing (..)
import Graphics.Collage
import Graphics.Collage exposing (Form, move, circle, filled)
import Mouse
import Time exposing (every, second, Time)

--This is the function you are trying to construct.
--It takes in a position, converts it to an element,
--using show and then converts it to a Form.
formPosition : (Int, Int) -> Form
formPosition pos =
  let element = show pos -- element : Element
  in Graphics.Collage.toForm element

-- We now want to apply our formPosition function to the
-- Signal containing all mouse position changes.
-- So we use Signal.map to apply formPosition to all values
-- of Mouse.position
formSignal : Signal Form
formSignal = Signal.map formPosition Mouse.position

-- Eventually we want to render this on the screen and the
-- function to do this requires a List Form not just a single
-- Form. So we write a function which returns a Singleton list
-- and apply it to each value in our formSignal.
formListSignal : Signal Form -> Signal (List Form)
formListSignal sf = Signal.map (\n -> [n]) sf

fieldSize = (400, 400)

circleAtTime : Int -> Form
circleAtTime t =
  let (sizex, sizey) = fieldSize
  in move (0, (toFloat (-1 * t % sizey))) (filled (rgb 100 100 100) (circle 20))

circleSignal : Signal Form
circleSignal = Signal.map circleAtTime (Signal.foldp (\tick total -> total + 1) 0 (every (0.01 * second)))

-- Finally, we must turn that into a Signal Element to render
-- on the screen. We partially apply Graphics.Collage.collage
-- to return an element of size 400x400 and apply it to the
-- values of formListSignal by using Signal.map again
elementSignal : Signal Element
elementSignal =
  let (x, y) = fieldSize
  in Signal.map (Graphics.Collage.collage x y) (formListSignal circleSignal)

-- Finally we hand this off to main and it renders
main : Signal Element
main = elementSignal