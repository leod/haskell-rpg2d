module Consts
    ( tileWidth
    , tileHeight
    , viewWidth
    , viewHeight
    , numLayers
    ) where

tileWidth, tileHeight :: (Num a) => a
tileWidth = 16
tileHeight = 16

viewWidth, viewHeight :: Int
viewWidth = 320
viewHeight = 240

numLayers :: Int
numLayers = 4

{-screenWidth = 640-}
{-screenHeight = 480-}
