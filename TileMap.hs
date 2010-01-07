module TileMap
    ( Tile
    , TileMap(..)
    , Layer(..)
    , isTileSet
    , renderTileMap
    , tmWidth
    , tmHeight
    , tmPixelSize
    , tmPixelWidth
    , tmPixelHeight
    , testMap
    ) where

import Data.Array
import Data.Maybe
import Control.Monad

import Util
import Consts
import Render
import Graphics.Rendering.OpenGL.GL as GL

undergroundLayer = 0
foregroundLayer = 1

type Tile = Maybe Point2
data Layer = Layer { layerTileset :: String
                   , layerTiles :: Array Point2 Tile
                   } deriving (Show, Read)
data TileMap = TileMap { tmLayers :: [Layer]
                       , tmSize :: Point2
                       } deriving (Show, Read)

testMap = TileMap { tmLayers = [ Layer { layerTileset = "ts.png"
                                       , layerTiles = underground }
                               , Layer { layerTileset = "ts.png"
                                       , layerTiles = foreground }
                               ]
                  , tmSize = tmSize
                  }
    where underground = array ((0, 0), tmSize)
                         [((x, y), Just (0, 8)) | x <- [0..px tmSize], y <- [0..py tmSize]]
          foreground = array ((0, 0), tmSize)
                         [((x, y), if x == 0 || y == 0 || x == px tmSize - 1 || y == py tmSize - 1 || abs (10 - x) < 3 && abs (10 - y) < 3 || x == 7 && y == 5
                                       then Just (29, 15)
                                       else Nothing)
                          | x <- [0..px tmSize], y <- [0..py tmSize]]
          tmSize = (20, 20)

isTileSet layer p tm = isJust $ (layerTiles $ tmLayers tm !! layer) ! p

-- Debugging
renderGrid :: TileMap -> IO ()
renderGrid tm = withColor (GL.Color4 0 0 0 0.4) $ GL.renderPrimitive GL.Lines $ do
    forM_ [0 .. w] (\x -> do
       GL.vertex $ GL.Vertex2 (fromIntegral $ x*tileWidth) (0 :: GLfloat)
       GL.vertex $ GL.Vertex2 (fromIntegral $ x*tileWidth) (fromIntegral $ h*tileHeight :: GLfloat))

    forM_ [0 .. h] (\y -> do
       GL.vertex $ GL.Vertex2 (0 :: GLfloat) (fromIntegral $ y*tileHeight)
       GL.vertex $ GL.Vertex2 (fromIntegral $ w*tileWidth) (fromIntegral $ y*tileHeight :: GLfloat))

    where (w, h) = tmSize tm

tmWidth :: TileMap -> Int
tmWidth = px . tmSize

tmHeight :: TileMap -> Int
tmHeight = px . tmSize

tmPixelSize :: TileMap -> Size2
tmPixelSize = (^*^ (tileWidth, tileHeight)) . tmSize

tmPixelWidth :: TileMap -> Int
tmPixelWidth = px . tmPixelSize

tmPixelHeight :: TileMap -> Int
tmPixelHeight = py . tmPixelSize

renderLayer :: SpriteMap -> Layer -> IO ()
renderLayer sm Layer { layerTiles = tilearray, layerTileset = tileset } =
    let tiles = assocs tilearray
    in withTexture (sprTexture spr) $ GL.renderPrimitive GL.Quads $ forM_ tiles tile
        
    where spr = tileset `getSprite` sm
          tile (_, Nothing) = return ()
          tile ((x', y'), Just (xTile, yTile)) =
               let (x, y) = (fromIntegral x' * tileWidth, fromIntegral y' * tileHeight) :: (GLfloat, GLfloat)
                   (cx, cy) = (fromIntegral $ tileWidth * xTile, fromIntegral $ tileHeight * yTile) :: (GLfloat, GLfloat)
                   (cw, ch) = (tileWidth, tileHeight) :: (GLfloat, GLfloat)
                   (tx, ty) = (sprWidthRatio spr * (cx / sprWidth spr),
                               sprHeightRatio spr * (cy / sprHeight spr))
                   (tw, th) = (sprWidthRatio spr * (cw / sprWidth spr),
                               sprHeightRatio spr * (ch / sprHeight spr))
               in do
                   GL.texCoord $ GL.TexCoord2 tx (ty+th)
                   GL.vertex   $ GL.Vertex2 x y

                   GL.texCoord $ GL.TexCoord2 (tw+tx) (ty+th)
                   GL.vertex   $ GL.Vertex2 (x+cw) y

                   GL.texCoord $ GL.TexCoord2 (tw+tx) ty
                   GL.vertex   $ GL.Vertex2 (x+cw) (y+ch)

                   GL.texCoord $ GL.TexCoord2 tx ty
                   GL.vertex   $ GL.Vertex2 x (y+ch)

renderTileMap :: TileMap -> SpriteMap -> IO ()
renderTileMap tm sm = do
    forM_ (tmLayers tm) $ renderLayer sm
    renderGrid tm
