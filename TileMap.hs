module TileMap (Tile, TileMap) where

import Data.Array

import Util

type Tile = Int
type TileMap = Array Point2 Tile

