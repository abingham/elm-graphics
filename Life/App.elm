module Life.App where

import Effects exposing (Effects)

import StartApp

import Life.Grid
import Life.Grid exposing (Grid)
import Life.Input exposing (..)
import Life.Util exposing (noFx, randomBools, ticks)
import Life.View exposing (view)

-- TODO: Use time for random seed, or let use specify it
-- TODO: Controls for grid size
-- TODO: Let user bring cells to life via clicking or something.
-- TODO: Control over cell size
-- TODO: Make it faster! Surely we can improve on this...

-- Aggregate of input types
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
