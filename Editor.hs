{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Control.Monad 
import Control.Monad.Trans (liftIO)
import Control.Arrow (first)
import Control.Applicative ((<$>))
import Data.IORef
import Data.Array.MArray
import Data.Array.IO
import qualified Data.Map as M
import Data.Map ((!))

import Graphics.UI.Gtk 
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Gdk.Drawable
import Graphics.UI.Gtk.Gdk.EventM (eventWindow, eventRegion)
import Graphics.UI.Gtk.Gdk.Events hiding (eventRegion)

import Util 
import Consts

-------------------------------------------------------------------------------
-- State
-------------------------------------------------------------------------------
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
data Layer = Layer { layerTileset :: String
                   , layerData :: IOArray Point2 (Maybe Tile)
                   }
type LayerId = Int

data Map = Map { mapSize :: Size2
               , mapLayers :: IOArray LayerId Layer
               }

data TilesetState = TilesetState { tsSelection :: Rect
                                 , tsMouseDown :: Bool
                                 , tsMouseStart :: Point2
                                 }

data MapWidgetState = MapWidgetState { msMouseDown :: Bool
                                     , msOldPos :: Point2
                                     }

type History = ([EditAction], [EditAction])

data State = State {
                   -- Currently selected layer
                     stLayer :: IORef LayerId

                   -- Tileset widget state
                   , stTileset :: IORef TilesetState

                   -- Undo/Redo
                   , stHistory :: IORef History

                   -- The map
                   , stMap :: IORef Map

                   -- Map widget state
                   , stMapWidgetState :: IORef MapWidgetState

                   -- Map of loaded pixbufs to avoid loading things twice
                   , stPixbufs :: IORef PixbufMap 

                   -- Widgets
                   , stTilesetWidget :: DrawingArea
                   , stMapWidget :: DrawingArea
                   }

type PixbufMap = M.Map String Pixbuf

pixbuf :: State -> String -> IO Pixbuf
pixbuf s name = readIORef (stPixbufs s) >>= return . (! name)

loadPixbuf :: PixbufMap -> String -> IO PixbufMap
loadPixbuf bufs name = do
    buf <- pixbufNewFromFile ("data/" ++ name)
    return $ M.insert name buf bufs 

loadPixbufs :: PixbufMap -> [String] -> IO PixbufMap
loadPixbufs = foldM loadPixbuf

-------------------------------------------------------------------------------
-- Edit actions
-------------------------------------------------------------------------------

-- Returns an action which reverts this action, or Nothing if no state was changed
newtype EditAction = EditAction { runEditAction :: Map -> IO (Maybe EditAction, Map) }

actionSetTiles :: LayerId -> [(Point2, Maybe Tile)] -> EditAction
actionSetTiles layerId tiles = EditAction $ \state -> do
    putStrLn "setting tiles" -- DEBUG

    let tiles' = filter (isValidPos state . fst) tiles
        layers = mapLayers state
    layer@Layer{ layerData = layerData } <- readArray layers layerId
    oldTiles <- mapM (\(ix, _) -> (,) ix <$> readArray layerData ix) tiles'
    forM tiles' (\(ix, t) -> writeArray layerData ix t)

    let undo = if oldTiles == tiles'
                   then Nothing
                   else Just $ actionSetTiles layerId oldTiles

    return (undo, state)

actionSetRectangle :: LayerId -> Point2 -> Rect -> EditAction
actionSetRectangle layerId (px, py) (Rect tx ty tw th) =
    let tiles = concatMap (\x -> map (\y -> ((px+x, py+y), Just (tx+x, ty+y))) [0..th-1]) [0..tw-1]
    in actionSetTiles layerId tiles

isValidPos :: Map -> Point2 -> Bool
isValidPos Map { mapSize = (sx, sy) } (px, py) =
    px >= 0 && py >= 0 && px < sx && py < sy

-------------------------------------------------------------------------------
-- Map widget
-------------------------------------------------------------------------------
drawMap w State{ stPixbufs, stMap } = do
    win <- eventWindow
    region <- eventRegion

    liftIO $ do
        putStrLn "drawing map" -- DEBUG

        pixbufs <- readIORef stPixbufs
        Map { mapLayers = layers, mapSize = (sizeX, sizeY) } <- readIORef stMap

        draw <- widgetGetDrawWindow w
        gc <- gcNew draw

        let drawLayer Layer { layerTileset = ts, layerData = dat } = do
            let tsPixbuf = pixbufs ! ts 

            -- TODO: cull
            doFromTo 0 (sizeX-1) $ \x ->
                doFromTo 0 (sizeY-1) $ \y -> do
                    t <- readArray dat (x, y)
                    case t of 
                        Just (tx, ty) ->
                            drawPixbuf draw gc tsPixbuf
                                       (tx*tileWidth) (ty*tileHeight)
                                       (x*tileWidth) (y*tileHeight)
                                       tileWidth tileHeight RgbDitherNone 0 0
                        _ -> return ()
        
        getElems layers >>= mapM drawLayer 
    
    return True

onMapSizeChange w State{ stMap } = do
   Map { mapSize = (sizeX, sizeY) } <- readIORef stMap

   widgetSetSizeRequest w (sizeX*tileWidth) (sizeY*tileHeight)

onMapButtonPress w s@State{ stTileset, stLayer, stMap, stHistory, stMapWidgetState } Button{ eventX, eventY } =
    let p = eventPosToTilePos eventX eventY
    in do
        rect <- readIORef stTileset >>= return . tsSelection
        layer <- readIORef stLayer
        recordAction s $ actionSetRectangle layer p rect
        modifyIORef stMapWidgetState $ \s -> s { msMouseDown = True, msOldPos = p }

        return True

onMapButtonRelease w State{ stMapWidgetState } _ =
    modifyIORef stMapWidgetState (\s -> s { msMouseDown = False }) >>
    return True
onMapLeave = onMapButtonRelease

onMapMotion w s@State{ stMapWidgetState, stTileset, stLayer } Motion{ eventX, eventY } =
    let p = eventPosToTilePos eventX eventY
    in do
        MapWidgetState { msMouseDown, msOldPos } <- readIORef stMapWidgetState

        when (msMouseDown && msOldPos /= p) $ do
            rect <- readIORef stTileset >>= return . tsSelection
            layer <- readIORef stLayer
            recordAction s $ actionSetRectangle layer p rect
            modifyIORef stMapWidgetState $ \s -> s { msOldPos = p }
        
        return True

-------------------------------------------------------------------------------
-- Tileset widget
-------------------------------------------------------------------------------
currentTileset :: State -> IO Pixbuf
currentTileset State{ stLayer, stPixbufs, stMap } = do
    layer <- readIORef stLayer
    pixbufs <- readIORef stPixbufs
    Map{ mapLayers } <- readIORef stMap
    Layer{ layerTileset } <- readArray mapLayers layer
    return $ pixbufs ! layerTileset

tilesetSize :: State -> IO Size2
tilesetSize s = do
    tileset <- currentTileset s
    w <- pixbufGetWidth tileset
    h <- pixbufGetHeight tileset
    return (w `div` tileWidth, h `div` tileHeight)

tilesetRectInBounds :: Point2 -> Rect -> Bool
tilesetRectInBounds (sizeX, sizeY) rect =
    rectX2 rect <= sizeX && rectY2 rect <= sizeY && rectX rect >= 0 && rectY rect >= 0

drawTileSet w s@State{ stTileset } = liftIO $ do
    putStrLn "drawing tileset" -- DEBUG

    TilesetState{ tsSelection = rect } <- readIORef stTileset
    tileset <- currentTileset s

    draw <- widgetGetDrawWindow w
    gc <- gcNew draw
    drawPixbuf draw gc tileset 0 0 0 0 (-1) (-1) RgbDitherNormal 0 0
    drawRectangle draw gc False (rectX rect * tileWidth) (rectY rect * tileWidth) (rectW rect * tileWidth) (rectH rect * tileHeight)

    return True

eventPosToTilePos evX evY = (round evX `div` tileWidth, round evY `div` tileHeight)

onTileSetButtonPress w s@State{ stTileset } Button{ eventX, eventY } =
    let p = eventPosToTilePos eventX eventY
    in do
        tsSize <- tilesetSize s
        let rect' = mkRect p (1, 1)

        when (tilesetRectInBounds tsSize rect') $ do
            modifyIORef stTileset (\s -> s { tsSelection = mkRect p (1, 1)
                                           , tsMouseDown = True
                                           , tsMouseStart = p
                                           })
            widgetQueueDraw w

        return True 
            
onTileSetMotion w s@State{ stTileset } Motion{ eventX, eventY } = do
    TilesetState { tsMouseDown = down
                 , tsMouseStart = (x, y)
                 , tsSelection = rect } <- readIORef stTileset
    tsSize <- tilesetSize s

    when down $ do
        let (x', y') = eventPosToTilePos eventX eventY 
            rect' = Rect (if x' < x then x' else x)
                         (if y' < y then y' else y)
                         (max 1 . (+1) . abs $ x' - x)
                         (max 1 . (+1) . abs $ y' - y)

        when (rect' /= rect && tilesetRectInBounds tsSize rect') $ do
            putStrLn "changing rect" -- DEBUG

            modifyIORef stTileset (\s -> s { tsSelection = rect' })
            widgetQueueDraw w
    
    return True

onTileSetButtonRelease w State{ stTileset } _ =
    modifyIORef stTileset (\s -> s { tsMouseDown = False }) >>
    return True
onTileSetLeave = onTileSetButtonRelease

-------------------------------------------------------------------------------
-- History
-------------------------------------------------------------------------------
onUndo w State{ stHistory, stMap, stMapWidget } = do
    map <- readIORef stMap
    ((action:tail), _) <- readIORef stHistory
    (revert, map') <- runEditAction action map
    writeIORef stMap map'
    modifyIORef stHistory (first $ const tail)
    widgetQueueDraw stMapWidget

-- Run an action and add an undo action for it
recordAction :: State -> EditAction -> IO ()
recordAction State { stHistory, stMap, stMapWidget } action = do
    map <- readIORef stMap 
    (revert, map') <- runEditAction action map
    case revert of
        Just a -> do
            writeIORef stMap map'
            modifyIORef stHistory $ first (a:)
            widgetQueueDraw stMapWidget
        Nothing -> return ()
        
-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------
main = do
    initGUI
    Just xml <- xmlNew "editor.glade"

    window <- xmlGetWidget xml castToWindow "window1"
    onDestroy window mainQuit

    tilesetDraw <- xmlGetWidget xml castToDrawingArea "tilesetDraw"
    mapDraw <- xmlGetWidget xml castToDrawingArea "mapDraw"
    
    mapState <- initMap >>= newIORef
    tsState <- newIORef TilesetState { tsSelection = Rect 0 0 1 1
                                     , tsMouseDown = False
                                     , tsMouseStart = (0, 0)
                                     }
    historyState <- newIORef ([], [])
    currentLayerState <- newIORef 0
    mapWidgetState <- newIORef MapWidgetState { msMouseDown = False, msOldPos = (0, 0) }
    
    pixbufs <- loadPixbufs M.empty ["test3.png"]
    pixbufState <- newIORef pixbufs

    let state = State { stTileset = tsState
                      , stLayer = currentLayerState
                      , stHistory = historyState
                      , stMap = mapState
                      , stTilesetWidget = tilesetDraw
                      , stMapWidget = mapDraw
                      , stPixbufs = pixbufState
                      , stMapWidgetState = mapWidgetState
                      }

    on mapDraw exposeEvent $ drawMap mapDraw state
    onButtonPress mapDraw $ onMapButtonPress mapDraw state
    onButtonRelease mapDraw $ onMapButtonRelease mapDraw state
    onLeaveNotify mapDraw $ onMapLeave mapDraw state
    onMotionNotify mapDraw True $ onMapMotion mapDraw state
    
    on tilesetDraw exposeEvent $ drawTileSet tilesetDraw state
    onButtonPress tilesetDraw $ onTileSetButtonPress tilesetDraw state
    onButtonRelease tilesetDraw $ onTileSetButtonRelease tilesetDraw state
    onLeaveNotify tilesetDraw $ onTileSetLeave tilesetDraw state
    onMotionNotify tilesetDraw True $ onTileSetMotion tilesetDraw state

    menuUndo <- xmlGetWidget xml castToMenuItem "menuUndo"
    onActivateLeaf menuUndo $ onUndo menuUndo state

    changeLayer 0 state
    onMapSizeChange mapDraw state

    widgetShowAll window
    mainGUI

initMap :: IO Map
initMap = do
    let size = (40, 40) 
        layerDim = ((0, 0), size^-1)

        emptyLayer tileset = do
            dat <- newListArray layerDim $ repeat Nothing
            return Layer { layerTileset = tileset, layerData = dat }

    undergroundDat <- newListArray layerDim (repeat $ Just (0, 0)) 
    let layer0 = Layer { layerTileset = "test3.png", layerData = undergroundDat }
    layer1 <- emptyLayer "test3.png"
    layer2 <- emptyLayer "test3.png"
    layer3 <- emptyLayer "test3.png"

    layers <- newListArray (0, numLayers-1) [layer0,layer1,layer2,layer3]

    return Map { mapSize = size
               , mapLayers = layers
               }

changeLayer :: LayerId -> State -> IO ()
changeLayer lid s@State{ stMap, stPixbufs, stTilesetWidget, stLayer, stTileset } = do
    Map { mapLayers = layers } <- readIORef stMap
    Layer { layerTileset = tileset } <- readArray layers lid

    writeIORef stLayer lid
    modifyIORef stTileset $ \s -> s { tsSelection = Rect 0 0 1 1 }

    tsBuf <- currentTileset s
    tsWidth <- pixbufGetWidth tsBuf
    tsHeight <- pixbufGetHeight tsBuf 
    widgetSetSizeRequest stTilesetWidget tsWidth tsHeight
