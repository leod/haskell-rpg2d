module MapData
    ( MapData(..)
    , Tile
    , Layer(..)
    , TileMap(..)
    , saveMap
    , loadMap
    ) 
    where

-- This module is used both by Main and Editor to serialize maps

import TileMap 

data MapData = MapData { mdTileMap :: TileMap
                       -- More to come (actors)
                       } deriving (Show, Read)

saveMap :: FilePath -> MapData -> IO ()
saveMap f = writeFile f . show

loadMap :: FilePath -> IO MapData
loadMap f = read `fmap` readFile f 
