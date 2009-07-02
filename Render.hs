module Render (Sprite, surfaceToSprite, sprite, SpriteMap, newSpriteMap, addSprite, getSprite) where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State.Strict
import System.Mem.Weak
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as Image
import Graphics.Rendering.OpenGL.GL as GL

import Util

type SpriteMap = Map String Sprite

newSpriteMap :: SpriteMap
newSpriteMap = Map.empty

addSprite :: String -> SpriteMap -> IO SpriteMap
addSprite name map =
    case name `Map.lookup` map of
        Just _ -> return map                  
        Nothing -> do spr <- loadSprite name
                      return $ Map.insert name spr map 

getSprite :: String -> SpriteMap -> Sprite
getSprite name map = let (Just spr) = name `Map.lookup` map
                     in spr

-- The following is taken mostly from Graphics.DrawingCombinators

data Sprite = Sprite { sprObject :: GL.TextureObject
                     , sprWidth :: Double
                     , sprHeight :: Double 

                     -- Ratio surface/padded surface
                     , sprWidthRatio :: Double
                     , sprHeightRatio :: Double
                     } deriving Show

nextPowerOf2 x = head $ dropWhile (< x) $ iterate (* 2) 1

-- Pad to n^2
padSurface :: SDL.Surface -> IO SDL.Surface
padSurface surf
    | newWidth == oldWidth && newHeight == oldHeight = return surf
    | otherwise = do
        surf' <- SDL.createRGBSurfaceEndian [] newWidth newHeight 32
        SDL.setAlpha surf [] 0xff
        SDL.blitSurface surf Nothing surf' Nothing
        return surf'
    where
        oldWidth = SDL.surfaceGetWidth surf
        oldHeight = SDL.surfaceGetHeight surf
        newWidth = nextPowerOf2 oldWidth
        newHeight = nextPowerOf2 oldHeight

-- Create OGL texture from SDL surface
surfaceToSprite :: SDL.Surface -> IO Sprite
surfaceToSprite surf = do
    surf' <- padSurface surf

    [tex@(GL.TextureObject b)] <- GL.genObjectNames 1
    --isGood <- GL.isObjectName tex
    --unless isGood $ fail "WUT"

    oldTex <- GL.get (GL.textureBinding GL.Texture2D)
    GL.textureBinding GL.Texture2D $= Just tex

    pixels <- SDL.surfaceGetPixels surf'
    bpp <- SDL.pixelFormatGetBytesPerPixel (SDL.surfaceGetPixelFormat surf')

    let pixelFormat = case bpp of
                          3 -> GL.RGB
                          4 -> GL.RGBA

    GL.textureFunction $= GL.Modulate
    GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
    GL.textureWrapMode GL.Texture2D GL.S $= (GL.Mirrored, GL.Repeat)
    GL.textureWrapMode GL.Texture2D GL.T $= (GL.Mirrored, GL.Repeat)

    GL.texImage2D Nothing GL.NoProxy 0 (GL.RGBA')
                  (GL.TextureSize2D (fromIntegral $ SDL.surfaceGetWidth surf')
                                    (fromIntegral $ SDL.surfaceGetHeight surf'))
                  0
                  (GL.PixelData pixelFormat GL.UnsignedByte pixels)

    GL.textureBinding GL.Texture2D $= oldTex

    let (w, w') = (SDL.surfaceGetWidth  surf, SDL.surfaceGetWidth  surf')
        (h, h') = (SDL.surfaceGetHeight surf, SDL.surfaceGetHeight surf') 

    let sprite = Sprite { sprObject = tex
                        , sprWidthRatio  = fromIntegral w / fromIntegral w'
                        , sprHeightRatio = fromIntegral h / fromIntegral h'
                        , sprWidth  = fromIntegral w
                        , sprHeight = fromIntegral h
                        }

    addFinalizer sprite $ do
        GL.deleteObjectNames [sprObject sprite]

    return sprite

loadSprite :: FilePath -> IO Sprite
loadSprite path = Image.load path >>= surfaceToSprite

sprite :: Sprite -> Point2 -> IO ()
sprite spr (ox, oy) = do
    oldTex <- GL.get (GL.textureBinding GL.Texture2D)

    GL.preservingMatrix $ do
        translate $ Vector3 (fromIntegral ox) (fromIntegral oy) (0 :: Double)

        GL.renderPrimitive GL.Quads $ do
            let (x, y) = (0.5 * sprWidth spr, 0.5 * sprWidth spr)
                (tx, ty) = (sprWidthRatio spr, sprHeightRatio spr)

            GL.texCoord $ GL.TexCoord2 0 (0 :: Double)
            GL.vertex   $ GL.Vertex2 (-x) y
            GL.texCoord $ GL.TexCoord2 tx 0
            GL.vertex   $ GL.Vertex2 x y
            GL.texCoord $ GL.TexCoord2 tx ty
            GL.vertex   $ GL.Vertex2 x (-y)
            GL.texCoord $ GL.TexCoord2 0 ty
            GL.vertex   $ GL.Vertex2 (-x) (-y)

    GL.textureBinding GL.Texture2D $= oldTex
