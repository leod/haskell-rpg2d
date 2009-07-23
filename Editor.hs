{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Control.Monad (when, forM, liftM, forever)
import Control.Monad.Trans (liftIO)
import Control.Applicative ((<$>))
import Data.IORef
import Data.Array.MArray
import Data.Array.IO

import Graphics.UI.Gtk hiding (Action)
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Gdk.Drawable
import Graphics.UI.Gtk.Gdk.EventM (eventWindow, eventRegion)
import Graphics.UI.Gtk.Gdk.Events hiding (eventRegion)

import Util 
import Consts

data ActorType = ActorType { atName :: String
                           , atProperties :: [String]
                           , atSprite :: String
                           }
type ActorTypeId = Int

data ActorInfo = ActorInfo { aTypeId :: ActorTypeId
                           , aProperties :: [(String, String)]
                           , aPos :: Point2
                           }

type Tile = Point2
data Layer = Layer { lyTileset :: String
                   , lyData :: IOArray Point2 (Maybe Tile)
                   }
type LayerId = Int

data MapState = MapState { msSize :: Size2
                         , msLayers :: IOArray LayerId Layer
                         }

-- Returns an action which reverts this action
newtype Action = Action (MapState -> IO (Action, MapState))

data State = State { stTileset :: Pixbuf
                   , stCurrentLayer :: Int
                   , stSelectedTiles :: Rect
                   , stTilesetMouseDown :: Bool
                   , stTilesetMouseStart :: Point2
                   , stHistory :: [Action]
                   , stMap :: MapState
                   }

isValidPos :: MapState -> Point2 -> Bool
isValidPos MapState { msSize = (sx, sy) } (px, py) =
    px >= 0 && py >= 0 && px < sx && py < sy

actionSetTiles :: LayerId -> [(Point2, Maybe Tile)] -> Action
actionSetTiles layerId tiles = Action $ \state -> do
    let tiles' = filter (isValidPos state . fst) tiles
        layers = msLayers state
    layer@Layer{ lyData = lyData } <- readArray layers layerId
    oldTiles <- mapM (\(ix, _) -> (,) ix <$> readArray lyData ix) tiles'
    forM tiles' (\(ix, t) -> writeArray lyData ix t)

    return (actionSetTiles layerId oldTiles, state)

actionSetRectangle :: LayerId -> Point2 -> Rect -> Action
actionSetRectangle layerId (px, py) (Rect tx ty tw th) =
    let tiles = concatMap (\x -> map (\y -> ((px+x, py+y), Just (tx+x, ty+y))) [0..th]) [0..tw]
    in actionSetTiles layerId tiles

drawTileSet w s = liftIO $ do
    State { stTileset = tileset, stSelectedTiles = rect } <- readIORef s

    draw <- widgetGetDrawWindow w
    gc <- gcNew draw
    drawPixbuf draw gc tileset 0 0 0 0 (-1) (-1) RgbDitherNone 0 0
    drawRectangle draw gc False (rectX rect * tileWidth) (rectY rect * tileWidth) (rectW rect * tileWidth) (rectH rect * tileHeight)
    return True

drawMap w s = do
    win <- eventWindow
    region <- eventRegion

    liftIO $ do
        ts <- stTileset <$> readIORef s

        draw <- widgetGetDrawWindow w
        gc <- gcNew draw
        
        drawPixbuf draw gc ts 0 0 0 0 32 32 RgbDitherNone 0 0
    
    return True

tilesetSize :: IORef State -> IO Size2
tilesetSize s = do
    State{ stTileset = tileset } <- readIORef s
    w <- pixbufGetWidth tileset
    h <- pixbufGetHeight tileset
    return (w `div` tileWidth, h `div` tileHeight)

tilesetRectInBounds :: Point2 -> Rect -> Bool
tilesetRectInBounds (sizeX, sizeY) rect =
    rectX2 rect <= sizeX && rectY2 rect <= sizeY && rectX rect >= 0 && rectY rect >= 0

eventPosToTilePos evX evY = (round evX `div` tileWidth, round evY `div` tileHeight)

onTileSetButtonPress w s Button { eventX, eventY } =
    let p = eventPosToTilePos eventX eventY
    in do
        tsSize <- tilesetSize s
        let rect' = mkRect p (1, 1)

        when (tilesetRectInBounds tsSize rect') $ do
            modifyIORef s (\s -> s { stSelectedTiles = mkRect p (1, 1)
                                   , stTilesetMouseDown = True
                                   , stTilesetMouseStart = p
                                   })
            widgetQueueDraw w
        return True 
            
onTileSetButtonRelease w s Button { } =
    modifyIORef s (\s -> s { stTilesetMouseDown = False }) >>
    return True

onTileSetMotion w s Motion { eventX, eventY } = do
    State { stTilesetMouseDown = down
          , stTilesetMouseStart = (x, y)
          , stSelectedTiles = rect
          } <- readIORef s
    tsSize <- tilesetSize s

    when down $ do
        let (x', y') = eventPosToTilePos eventX eventY 
            rect' = Rect (if x' < x then x' else x)
                         (if y' < y then y' else y)
                         (max 1 . (+1) . abs $ x' - x)
                         (max 1 . (+1) . abs $ y' - y)

        when (rect' /= rect && tilesetRectInBounds tsSize rect') $ do
            modifyIORef s (\s -> s { stSelectedTiles = rect' })
            widgetQueueDraw w
    
    return True

onTileSetLeave w s Crossing { eventCrossingMode = m } = 
    modifyIORef s (\s -> s { stTilesetMouseDown = False }) >>
    return True

initMapState :: IO MapState
initMapState = do
    let size = (40, 40) 
        layerDim = ((0, 0), size)
    underground <- newListArray layerDim (repeat $ Just (0, 0)) 
    otherLayers <- forever $ newListArray layerDim (repeat Nothing)
    layers <- newListArray (0, numLayers) (underground:otherLayers)

    return MapState { msSize = size
                    , msLayers = layers
                    }

main = do
    initGUI
    Just xml <- xmlNew "editor.glade"

    window <- xmlGetWidget xml castToWindow "window1"
    onDestroy window mainQuit

    tileset <- pixbufNewFromFile "data/test3.png"

    state <- newIORef State { stTileset = tileset
                            , stSelectedTiles = mkRect (3, 1) (1, 1)
                            , stTilesetMouseDown = False
                            , stTilesetMouseStart = (0, 0)
                            , stCurrentLayer = 0
                            , stHistory = []
                            }

    mapDraw <- xmlGetWidget xml castToDrawingArea "mapDraw"
    on mapDraw exposeEvent $ drawMap mapDraw state
    
    tilesetDraw <- xmlGetWidget xml castToDrawingArea "tilesetDraw"
    on tilesetDraw exposeEvent $ drawTileSet tilesetDraw state
    onButtonPress tilesetDraw $ onTileSetButtonPress tilesetDraw state
    onButtonRelease tilesetDraw $ onTileSetButtonRelease tilesetDraw state
    onLeaveNotify tilesetDraw $ onTileSetLeave tilesetDraw state
    onMotionNotify tilesetDraw True $ onTileSetMotion tilesetDraw state

    tsWidth <- pixbufGetWidth tileset
    tsHeight <- pixbufGetHeight tileset
    widgetSetSizeRequest tilesetDraw tsWidth tsHeight

    widgetShowAll window
    mainGUI
