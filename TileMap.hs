module TileMap (Tile, TileMap, renderTileMap) where

import Data.Array

import Util
import Render
import Graphics.Rendering.OpenGL.GL as GL

type Tile = Int
type TileMap = Array Point2 Tile

renderTileMap :: TileMap -> SpriteMap -> IO ()
renderTileMap tm sm = {-# SCC "renderTileMap" #-}
                      let tiles = assocs tm
                          spr = "tilemap.bmp" `getSprite` sm
                      in withTexture (sprTexture spr) $ GL.renderPrimitive GL.Quads $ do 
                        mapM_ (\((x', y'), t) ->
                               let (x, y) = ((fromIntegral x')*16, (fromIntegral y')*16) :: (Double, Double)
                                   (cx, cy) = (16, 16) :: (Double, Double)
                                   (cw, ch) = (16, 16) :: (Double, Double)
                                   (tx, ty) = (sprWidthRatio spr * (cx / (sprWidth spr)),
                                               sprHeightRatio spr * (cy / (sprHeight spr)))
                                   (tw, th) = (sprWidthRatio spr * (cw / (sprWidth spr)),
                                               sprHeightRatio spr * (ch / (sprHeight spr)))
                               in do
                                   GL.texCoord $ GL.TexCoord2 tx ty
                                   GL.vertex   $ GL.Vertex2 x y

                                   GL.texCoord $ GL.TexCoord2 (tw+tx) tx
                                   GL.vertex   $ GL.Vertex2 (x+cw) y

                                   GL.texCoord $ GL.TexCoord2 (tw+tx) (th+ty)
                                   GL.vertex   $ GL.Vertex2 (x+cw) (y+ch)

                                   GL.texCoord $ GL.TexCoord2 tx (th+ty)
                                   GL.vertex   $ GL.Vertex2 x (y+ch)) tiles
