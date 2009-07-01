module Resource (Renderer, image, ImageMap, ResId, runRenderer, newImageMap, drawImage) where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import Graphics.UI.SDL

import Util

type ResId = String
type ResMap a = Map ResId a

type ImageMap = ResMap Surface

type Renderer a = StateT ImageMap IO a

newImageMap :: ImageMap
newImageMap = Map.empty

image :: ResId -> Renderer Surface
image name = do res <- get
                case name `Map.lookup` res of
                    (Just a) -> return a
                    Nothing -> do surface <- liftIO $ loadBMP name
                                  put (Map.insert name surface res)
                                  return surface

drawImage :: ResId -> Point2 -> Surface ->  Renderer ()
drawImage name (x, y) sur = do img <- image name
                               b <- liftIO $ blitSurface img Nothing sur (Just $ Rect x y 0 0)
                               return ()

runRenderer :: Renderer a -> ImageMap -> IO (a, ImageMap)
runRenderer r = runStateT r
