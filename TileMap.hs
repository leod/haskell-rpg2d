module TileMap (Tile, TileMap, drawTileMap) where

import Data.Array

import Util
import Render
import Graphics.UI.SDL

type Tile = Int
type TileMap = Array Point2 Tile

drawTileMap :: TileMap -> Surface -> Surface -> IO ()
drawTileMap tm img sur = let tiles = assocs tm
                             rect = Rect 0 0 16 16 
                         in mapM_ (\((x, y), t) ->
                                   blitSurface img (Just rect) sur (Just $ Rect (x*16) (y*16) 0 0)) tiles
