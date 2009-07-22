{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Control.Applicative ((<$>))
import Data.IORef

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Gdk.Drawable
import Graphics.UI.Gtk.Gdk.EventM (eventWindow, eventRegion)
import Graphics.UI.Gtk.Gdk.Events hiding (eventRegion)

import Util 
import Consts

data State = State { tileset :: Pixbuf
                   , selectedTiles :: Rect
                   , tilesetMouseDown :: Bool
                   , tilesetMouseStart :: Point2
                   }

drawTileSet w s = liftIO $ do
    State { tileset, selectedTiles } <- readIORef s

    draw <- widgetGetDrawWindow w
    gc <- gcNew draw
    drawPixbuf draw gc tileset 0 0 0 0 (-1) (-1) RgbDitherNone 0 0
    drawRectangle draw gc False (rectX selectedTiles * tileWidth) (rectY selectedTiles * tileWidth) (rectW selectedTiles * tileWidth) (rectH selectedTiles * tileHeight)
    return True

drawMap w s = do
    win <- eventWindow
    region <- eventRegion

    liftIO $ do
        ts <- tileset <$> readIORef s

        draw <- widgetGetDrawWindow w
        gc <- gcNew draw
        
        drawPixbuf draw gc ts 0 0 0 0 32 32 RgbDitherNone 0 0
    
    return True

tilesetSize :: IORef State -> IO Size2
tilesetSize s = do
    State { tileset } <- readIORef s
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
            modifyIORef s (\s -> s { selectedTiles = mkRect p (1, 1)
                                   , tilesetMouseDown = True
                                   , tilesetMouseStart = p
                                   })
            widgetQueueDraw w
        return True 
            
onTileSetButtonRelease w s Button { } =
    modifyIORef s (\s -> s { tilesetMouseDown = False }) >>
    return True

onTileSetMotion w s Motion { eventX, eventY } = do
    State { tilesetMouseDown = down
          , tilesetMouseStart = (x, y)
          , selectedTiles = rect
          } <- readIORef s
    tsSize <- tilesetSize s

    when down $ do
        let (x', y') = eventPosToTilePos eventX eventY 
            rect' = Rect (if x' < x then x' else x)
                         (if y' < y then y' else y)
                         (max 1 . (+1) . abs $ x' - x)
                         (max 1 . (+1) . abs $ y' - y)

        when (rect' /= rect && tilesetRectInBounds tsSize rect') $ do
            modifyIORef s (\s -> s { selectedTiles = rect' })
            widgetQueueDraw w
    
    return True

onTileSetLeave w s Crossing { eventCrossingMode = m } = 
    modifyIORef s (\s -> s { tilesetMouseDown = False }) >>
    return True

main = do
    initGUI
    Just xml <- xmlNew "editor.glade"

    window <- xmlGetWidget xml castToWindow "window1"
    onDestroy window mainQuit

    tileset <- pixbufNewFromFile "data/test3.png"

    state <- newIORef State { tileset = tileset
                            , selectedTiles = mkRect (3, 1) (1, 1)
                            , tilesetMouseDown = False
                            , tilesetMouseStart = (0, 0)
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
