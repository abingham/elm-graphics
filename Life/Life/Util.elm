module Life.Util where

import Effects exposing (Effects)
import List
import Random
import Time

randomBools : Int -> Random.Seed -> List Bool
randomBools size seed =
  let
    gen = Random.list size Random.bool
    (vals, seed) = Random.generate gen seed
  in
    vals

ticks : Int -> Signal Int
ticks fps = (Signal.foldp (\tick total -> total + 1) 0 (Time.fps fps))

noFx : model -> (model, Effects a)
noFx model = (model, Effects.none)

chunks : Int -> List a -> List (List a)
chunks size l =
  if List.isEmpty l then
    []
  else
    (List.take size l) :: (chunks size (List.drop size l))
