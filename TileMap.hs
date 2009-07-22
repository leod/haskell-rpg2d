module TileMap
    ( Tile
    , TileMap
    , renderTileMap
    , mapSize
    , mapWidth
    , mapHeight
    , mapPixelSize
    , mapPixelWidth
    , mapPixelHeight
    , testMap
    ) where

import Data.Array
import Control.Monad

import Util
import Consts
import Render
import Graphics.Rendering.OpenGL.GL as GL

type Tile = Maybe Point2
type Layer = Array Point2 Tile
--data TileMap = TileMap Layer Layer Layer Layer
data TileMap = TileMap { underground :: Layer
                       , foreground :: Layer
                       , mapSize :: Point2
                       } deriving Show

testMap = TileMap { underground = underground'
                  , foreground = foreground'
                  , mapSize = mapSize'
                  }
    where underground' = array ((0, 0), mapSize')
                         [((x, y), Just (0, 0)) | x <- [0..px mapSize'], y <- [0..py mapSize']]
          foreground' = array ((0, 0), mapSize')
                         [((x, y), if (x == 0 || y == 0 || x == px mapSize' - 1 || y == py mapSize' - 1) then Just (0, 5) else Nothing) | x <- [0..px mapSize'], y <- [0..py mapSize']]
          mapSize' = (20, 20)

-- Debugging
renderGrid :: TileMap -> IO ()
renderGrid tm = withColor (GL.Color4 0 0 1 0.2) $ GL.renderPrimitive GL.Lines $ do
    forM_ [0 .. w] (\x -> do
       GL.vertex $ GL.Vertex2 (fromIntegral $ x*tileWidth) (0::Double)
       GL.vertex $ GL.Vertex2 (fromIntegral $ x*tileWidth) (fromIntegral $ h*tileHeight :: Double))

    forM_ [0 .. h] (\y -> do
       GL.vertex $ GL.Vertex2 (0::Double) (fromIntegral $ y*tileHeight)
       GL.vertex $ GL.Vertex2 (fromIntegral $ w*tileWidth) (fromIntegral $ y*tileHeight :: Double))

    where (w, h) = mapSize tm

mapWidth :: TileMap -> Int
mapWidth = px . mapSize

mapHeight :: TileMap -> Int
mapHeight = px . mapSize

mapPixelSize :: TileMap -> Size2
mapPixelSize = (^*^ (tileWidth, tileHeight)) . mapSize

mapPixelWidth :: TileMap -> Int
mapPixelWidth = px . mapPixelSize

mapPixelHeight :: TileMap -> Int
mapPixelHeight = py . mapPixelSize

renderLayer :: Layer -> SpriteMap -> IO ()
renderLayer layer sm =
    let tiles = assocs layer
    in withTexture (sprTexture spr) $ GL.renderPrimitive GL.Quads $ forM_ tiles tile
        
    where spr = "test3.png" `getSprite` sm
          tile (_, Nothing) = return ()
          tile ((x', y'), Just (xTile, yTile)) =
               let (x, y) = (fromIntegral x' * tileWidth, fromIntegral y' * tileHeight) :: (Double, Double)
                   (cx, cy) = (fromIntegral $ tileWidth * xTile, fromIntegral $ tileHeight * yTile) :: (Double, Double)
                   (cw, ch) = (tileWidth, tileHeight) :: (Double, Double)
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
    renderLayer (underground tm) sm
    renderLayer (foreground tm) sm
    renderGrid tm
