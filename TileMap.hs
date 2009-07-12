module TileMap (Tile, TileMap, renderTileMap) where

import Data.Array
import Control.Monad

import Util
import Consts
import Render
import Graphics.Rendering.OpenGL.GL as GL

type Tile = Int
type Layer = Array Point2 Tile
--data TileMap = TileMap Layer Layer Layer Layer
type TileMap = Layer

-- Debugging
renderGrid :: TileMap -> IO ()
renderGrid tm = withColor (GL.Color4 0 0 1 0.2) $ GL.renderPrimitive GL.Lines $ do
    forM_ [0 .. w] (\x -> do
       GL.vertex $ GL.Vertex2 (fromIntegral $ x*tileWidth) (0::Double)
       GL.vertex $ GL.Vertex2 (fromIntegral $ x*tileWidth) (fromIntegral $ h*tileHeight :: Double))

    forM_ [0 .. h] (\y -> do
       GL.vertex $ GL.Vertex2 (0::Double) (fromIntegral $ y*tileHeight)
       GL.vertex $ GL.Vertex2 (fromIntegral $ w*tileWidth) (fromIntegral $ y*tileHeight :: Double))

    where
        ((_, _), (w, h)) = bounds tm

renderTileMap :: TileMap -> SpriteMap -> IO ()
renderTileMap tm sm =
    let tiles = assocs tm
        spr = "ts.png" `getSprite` sm
    in (withTexture (sprTexture spr) $ GL.renderPrimitive GL.Quads $
        forM_ tiles (\((x', y'), t) ->
               let (x, y) = (fromIntegral x' * tileWidth, fromIntegral y' * tileHeight) :: (Double, Double)
                   (cx, cy) = (tileWidth*7, tileHeight*2) :: (Double, Double)
                   (cw, ch) = (tileWidth, tileHeight) :: (Double, Double)
                   (tx, ty) = (sprWidthRatio spr * (cx / sprWidth spr),
                               sprHeightRatio spr * (cy / sprHeight spr))
                   (tw, th) = (sprWidthRatio spr * (cw / sprWidth spr),
                               sprHeightRatio spr * (ch / sprHeight spr))
               in do
                   GL.texCoord $ GL.TexCoord2 tx ty
                   GL.vertex   $ GL.Vertex2 x y

                   GL.texCoord $ GL.TexCoord2 (tw+tx) ty
                   GL.vertex   $ GL.Vertex2 (x+cw) y

                   GL.texCoord $ GL.TexCoord2 (tw+tx) (th+ty)
                   GL.vertex   $ GL.Vertex2 (x+cw) (y+ch)

                   GL.texCoord $ GL.TexCoord2 tx (th+ty)
                   GL.vertex   $ GL.Vertex2 x (y+ch)))
        >> renderGrid tm
