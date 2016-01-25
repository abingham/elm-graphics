module Life.Model where

import Life.Grid

type alias Model =
  { grid : Life.Grid.Grid
  , cell_size : Int
  , seed : Int
  }

create : Int -> Int -> Int -> Int -> Model
create num_rows num_cols seed cell_size =
  { grid = Life.Grid.create num_rows num_cols seed
  , cell_size = cell_size
  , seed = seed
  }
