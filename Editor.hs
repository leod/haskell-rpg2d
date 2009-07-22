{-# LANGUAGE NamedFieldPuns #-}
module Main where

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

onTileSetClick s Button { eventX, eventY } = do
    print $ (eventX, eventY) 
    return True 

main = do
    initGUI
    Just xml <- xmlNew "editor.glade"

    window <- xmlGetWidget xml castToWindow "window1"
    onDestroy window mainQuit

    tileset <- pixbufNewFromFile "data/test3.png"

    state <- newIORef State { tileset = tileset
                            , selectedTiles = mkRect (3, 1) (1, 1)
                            }

    mapDraw <- xmlGetWidget xml castToDrawingArea "mapDraw"
    on mapDraw exposeEvent $ drawMap mapDraw state
    
    tileSetDraw <- xmlGetWidget xml castToDrawingArea "tileSetDraw"
    on tileSetDraw exposeEvent $ drawTileSet tileSetDraw state
    onButtonPress tileSetDraw $ onTileSetClick state

    tsWidth <- pixbufGetWidth tileset
    tsHeight <- pixbufGetHeight tileset
    widgetSetSizeRequest tileSetDraw tsWidth tsHeight

    widgetShowAll window
    mainGUI
