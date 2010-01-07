module Render
    ( Sprite
    , SpriteMap, newSpriteMap , addSprites, addSprite, getSprite
    , rectangle
    , emptySprite, renderToSprite
    , sprTexture, sprWidth, sprHeight, sprWidthRatio, sprHeightRatio
    , surfaceToSprite, loadSprite
    , spriteClipped, sprite
    , withTexture
    , withColor
    ) where

import Data.Map (Map)
import Data.Maybe
import Control.Monad
import qualified Data.Map as Map
import System.Mem.Weak
import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as Image
import Graphics.Rendering.OpenGL.GL (($=), Vector3(..), Vertex3(..), GLfloat)
import qualified Graphics.Rendering.OpenGL.GL as GL
import Graphics.Rendering.OpenGL.GLU as GLU
import Foreign.Ptr (nullPtr)

import Util

type SpriteMap = Map String Sprite

newSpriteMap :: SpriteMap
newSpriteMap = Map.empty

addSprite :: SpriteMap -> String -> IO SpriteMap
addSprite map name =
    case name `Map.lookup` map of
        Just _ -> return map                  
        Nothing -> do spr <- loadSprite name
                      return $ Map.insert name spr map 

addSprites :: SpriteMap -> [String] -> IO SpriteMap
addSprites = foldM addSprite

getSprite :: String -> SpriteMap -> Sprite
getSprite name = fromMaybe (error name) . Map.lookup name

rectangle :: GL.Color4 GLfloat -> Rect -> IO ()
rectangle c (Rect x y w h) = withColor c $ GL.renderPrimitive GL.LineLoop $ do
    GL.vertex $ Vertex3 x1 y1 0
    GL.vertex $ GL.Vertex3 x2 y1 0
    GL.vertex $ GL.Vertex3 x2 y2 0
    GL.vertex $ GL.Vertex3 x1 y2 0
    
    where x1 = fromIntegral x :: GLfloat
          y1 = fromIntegral y :: GLfloat
          x2 = x1 + fromIntegral w :: GLfloat
          y2 = y1 + fromIntegral h :: GLfloat

emptySprite :: Int -> Int -> IO Sprite
emptySprite w h = do
    [tex] <- GL.genObjectNames 1

    let w' = nextPowerOf2 w
        h' = nextPowerOf2 h
    
    withTexture tex $ do
        GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)
        --GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
        GL.texImage2D Nothing GL.NoProxy 0 GL.RGB'
                      (GL.TextureSize2D (fromIntegral w') (fromIntegral h'))
                      0
                      (GL.PixelData GL.RGB GL.UnsignedByte nullPtr)

    let sprite = Sprite { sprTexture = tex
                        , sprWidthRatio = fromIntegral w / fromIntegral w'
                        , sprHeightRatio = fromIntegral h / fromIntegral h'
                        , sprWidth = fromIntegral w
                        , sprHeight = fromIntegral h
                        }

    addFinalizer sprite $ do
        putStrLn "freeing empty texture"
        GL.deleteObjectNames [sprTexture sprite]

    return sprite

renderToSprite :: Sprite -> IO a -> IO a
renderToSprite spr act =
    GL.preservingAttrib [GL.ViewportAttributes] $ do
        GL.viewport $= (GL.Position 0 0, GL.Size (floor . sprWidth $ spr)
                                                 (floor . sprHeight $ spr))
        res <- act
        withTexture (sprTexture spr) $
            GL.copyTexSubImage2D Nothing 0 (GL.TexturePosition2D 0 0) (GL.Position 0 0)
                                 (GL.TextureSize2D (floor . sprWidth $ spr)
                                                   (floor . sprHeight $ spr))
        return res

-- The following is taken mostly from Graphics.DrawingCombinators

data Sprite = Sprite { sprTexture :: GL.TextureObject
                     , sprWidth :: GLfloat
                     , sprHeight :: GLfloat 

                     -- Ratio surface/padded surface
                     , sprWidthRatio :: GLfloat
                     , sprHeightRatio :: GLfloat
                     } deriving Show

nextPowerOf2 x = head . dropWhile (< x) . iterate (* 2) $ 1

-- Pad to n^2
padSurface :: SDL.Surface -> IO SDL.Surface
padSurface surf
    | newWidth == oldWidth && newHeight == oldHeight = return surf
    | otherwise = do
        surf' <- SDL.createRGBSurfaceEndian [] newWidth newHeight 32
        SDL.setAlpha surf [] 0xff
        SDL.blitSurface surf Nothing surf' Nothing
        return surf'
    where oldWidth = SDL.surfaceGetWidth surf
          oldHeight = SDL.surfaceGetHeight surf
          newWidth = nextPowerOf2 oldWidth
          newHeight = nextPowerOf2 oldHeight

-- Create OGL texture from SDL surface
surfaceToSprite :: SDL.Surface -> IO Sprite
surfaceToSprite surf = do
    surf' <- padSurface surf

    [tex] <- GL.genObjectNames 1

    oldTex <- GL.get (GL.textureBinding GL.Texture2D)
    GL.textureBinding GL.Texture2D $= Just tex

    pixels <- SDL.surfaceGetPixels surf'
    bpp <- SDL.pixelFormatGetBytesPerPixel (SDL.surfaceGetPixelFormat surf')

    let pixelFormat = case bpp of
                          3 -> GL.RGB
                          4 -> GL.RGBA

    GL.textureFunction $= GL.Modulate
    GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
    GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)
    GL.textureWrapMode GL.Texture2D GL.S $= (GL.Mirrored, GL.Repeat)
    GL.textureWrapMode GL.Texture2D GL.T $= (GL.Mirrored, GL.Repeat)

    GL.texImage2D Nothing GL.NoProxy 0 GL.RGBA'
                  (GL.TextureSize2D (fromIntegral $ SDL.surfaceGetWidth surf')
                                    (fromIntegral $ SDL.surfaceGetHeight surf'))
                  0
                  (GL.PixelData pixelFormat GL.UnsignedByte pixels)

    GL.textureBinding GL.Texture2D $= oldTex

    let (w, w') = (SDL.surfaceGetWidth  surf, SDL.surfaceGetWidth  surf')
        (h, h') = (SDL.surfaceGetHeight surf, SDL.surfaceGetHeight surf') 

    let sprite = Sprite { sprTexture = tex
                        , sprWidthRatio  = fromIntegral w / fromIntegral w'
                        , sprHeightRatio = fromIntegral h / fromIntegral h'
                        , sprWidth  = (fromIntegral w / fromIntegral w) * fromIntegral w
                        , sprHeight = (fromIntegral h / fromIntegral h) * fromIntegral h
                        }

    addFinalizer sprite $ do
        putStrLn "freeing texture"
        GL.deleteObjectNames [sprTexture sprite]

    return sprite

loadSprite :: FilePath -> IO Sprite
loadSprite path = putStrLn ("loading sprite " ++ path) >>
                  Image.load ("data/" ++ path) >>= surfaceToSprite

withTexture :: GL.TextureObject -> IO a -> IO a
withTexture tex act = do
    GL.texture GL.Texture2D $= GL.Enabled
    GL.textureBinding GL.Texture2D $= Just tex 
    res <- act
    GL.texture GL.Texture2D $= GL.Disabled
    return res

withTranslate :: Point2 -> IO a -> IO a
withTranslate (x, y) act =
    GL.preservingMatrix $ do
        GL.translate $ Vector3 (fromIntegral x) (fromIntegral y) (0 :: GLfloat)
        act

withColor :: GL.Color4 GLfloat -> IO a -> IO a
withColor c act = do
    GL.color c
    res <- act
    GL.color $ GL.Color4 1 1 1 (1 :: GLfloat)
    return res

spriteClipped :: Sprite -> Point2 -> Point2 -> Point2 -> IO ()
spriteClipped spr p (cx', cy') (cw', ch') =
    withTexture (sprTexture spr) $ withTranslate p $ GL.renderPrimitive GL.Quads $ do
        let (cx, cy) = (fromIntegral cx', fromIntegral cy')
            (cw, ch) = (fromIntegral cw', fromIntegral ch')
            (tx, ty) = (sprWidthRatio spr * (cx / sprWidth spr),
                        sprHeightRatio spr * (cy / sprHeight spr))
            (tw, th) = (sprWidthRatio spr * (cw / sprWidth spr),
                        sprHeightRatio spr * (ch / sprHeight spr))

        GL.texCoord $ GL.TexCoord2 tx (th+ty)
        GL.vertex   $ GL.Vertex2 0 (0 :: GLfloat)

        GL.texCoord $ GL.TexCoord2 (tw+tx) (th+ty)
        GL.vertex   $ GL.Vertex2 cw 0

        GL.texCoord $ GL.TexCoord2 (tw+tx) ty
        GL.vertex   $ GL.Vertex2 cw ch

        GL.texCoord $ GL.TexCoord2 tx ty
        GL.vertex   $ GL.Vertex2 0 ch

sprite :: Sprite -> Point2 -> IO ()
sprite spr p = spriteClipped spr p (0, 0) (truncate $ sprWidth spr, truncate $ sprHeight spr)
