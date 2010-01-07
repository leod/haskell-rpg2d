{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Array
import Control.Monad
import Control.Monad.Writer
import System.Random
import qualified Data.Map as Map
import qualified Graphics.UI.SDL as SDL
import Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.FTGL as FTGL
import Foreign (Word32)
import Data.List (foldl')
import Control.Arrow ((***))

import Actor
import Input
import TileMap
import GameState
import Consts
import Render
import Input
import Util 
import qualified IdentityList as IL
import IdentityList ((+:))

import Actor.Player
import Actor.Enemy
import Actor.Arrow

screenWidth, screenHeight :: Num a => a
screenWidth = 1024
screenHeight = 768

clampCamera :: GameState -> Point2 -> Point2
clampCamera gs = min maxX . max 0 *** min maxY . max 0 
    where tm = gsTileMap gs
          maxX = tmPixelWidth  tm - viewWidth
          maxY = tmPixelHeight tm - viewHeight

processEvents :: GameState -> [Event] -> GameState
processEvents = foldl' f
    where f :: GameState -> Event -> GameState
          f gs (AddActor a) = gs { gsActors = a +: gsActors gs  }
          f gs (RemoveActor id) = gs { gsActors =id `IL.delete` gsActors gs }
          f gs (MoveCamera p) = gs { gsCamera = clampCamera gs p }
          f gs _ = gs

debugEvents :: [Event] -> [String]
debugEvents = filter (/= "") . map f
    where f (Debug id str) = "Actor #" ++ show id ++ ": " ++ str
          f _ = ""

getMessages :: [Event] -> [MessageRec]
getMessages = foldl' f []
    where f :: [MessageRec] -> Event -> [MessageRec]
          f msgs (SendMessage msg) = msg : msgs
          f msgs _ = msgs

data MainState = MainState { msGameState :: GameState
                           , msSprites :: SpriteMap
                           , msInput :: Input
                           , msFont :: FTGL.Font
                           , msSurface :: Sprite
                           , msScreenW :: Int
                           , msScreenH :: Int
                           }

pollEvents :: IO [SDL.Event]
pollEvents = poll []
    where poll evs = do
              ev <- SDL.pollEvent

              case ev of
                  SDL.NoEvent -> return evs
                  _ -> poll $ ev : evs

updateGS :: GameState -> Input -> (GameState, [String])
updateGS gs input =
    let actors = gsActors gs
        random = gsRandom gs
        tm     = gsTileMap gs
        msgs   = gsMessages gs

        -- Update actors, resolve collisions and then dispatch messages
        ustate = UpdateState { usTileMap = tm
                             , usInput = input
                             , usSelfId = error "self id not set"
                             }
        act = do (actors', evs) <- listen $ updateActors actors >>= collisions 
                 dispatchMessages actors' $ getMessages evs 

        (actors'', !random', evs) = runAct act random ustate

        -- Process events
        gs' = gs { gsActors = actors'', gsRandom = random' }
                  `processEvents` evs
    in (gs', debugEvents evs)

renderMS :: MainState -> IO ()
renderMS MainState { msGameState = gs
                   , msSprites = sprs
                   , msFont = font
                   , msSurface = surface
                   , msScreenW = screenW
                   , msScreenH = screenH
                   } = do
    GL.blend $= GL.Enabled
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

    -- First render the scene to a 320x240 texture
    GL.clear [GL.ColorBuffer]

    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GL.ortho 0 (fromIntegral viewWidth) 0 (fromIntegral viewHeight) 0 128

    GL.matrixMode $= GL.Modelview 0
    GL.loadIdentity

    GL.preservingMatrix $ renderToSprite surface $ do
        GL.translate $ Vector3 (fromIntegral . negate . px $ gsCamera gs)
                               (fromIntegral . negate . py $ gsCamera gs)
                               (0 :: GL.GLfloat)

        renderTileMap (gsTileMap gs) sprs
        renderActors (gsActors gs) sprs

        GL.matrixMode $= GL.Modelview 0
        GL.loadIdentity
        
    -- Now render the texture, upscaled
    GL.clear [GL.ColorBuffer]

    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GL.ortho 0 (fromIntegral screenW) (fromIntegral screenH) 0 0 128

    GL.matrixMode $= GL.Modelview 0
    GL.loadIdentity
    GL.scale (fromIntegral screenW / fromIntegral viewWidth)
             (fromIntegral screenH / fromIntegral viewHeight)
             (1 :: GL.GLdouble)
    
    sprite surface (0, 0)
    {-renderTileMap (gsTileMap gs) sprs-}
    {-renderActors (gsActors gs) sprs-}


    SDL.glSwapBuffers

mainLoop :: MainState -> (Word32, Int) -> IO ()
mainLoop mstate (time, frames) = do
    sdlEvents <- pollEvents

    -- Update
    let input = updateInput (msInput mstate) sdlEvents

        (gstate, debug) = updateGS (msGameState mstate) input
        mstate' = mstate { msGameState = gstate
                         , msInput = input } 

        sprites = msSprites mstate'
        actors  = gsActors gstate
        random  = gsRandom gstate
        tm      = gsTileMap gstate
        msgs    = gsMessages gstate

    -- Debug events
    mapM_ putStrLn debug

    -- Render
    renderMS mstate'

    time' <- SDL.getTicks

    when (time' - time > 1000) $
        putStrLn $ "FPS: " ++ show frames

    let frmCtr = if (time' - time) > 1000
                     then (time', 0)
                     else (time, frames+1)

    SDL.delay 20

    if inQuit input
        then return ()
        else mainLoop mstate' frmCtr

main = do
    SDL.init [SDL.InitEverything]
    SDL.setVideoMode screenWidth screenHeight 32 [SDL.OpenGL]

    GL.clearColor $= GL.Color4 0 0 0 0
    GL.viewport $= (GL.Position 0 0, GL.Size screenWidth screenHeight)

    sprs <- newSpriteMap `addSprites` ["test2.png", "npc.bmp", "linkanim.png", "test.png", "enemy.png",
                                       "tileset.png", "ts.png", "arrow.png", "enemy2.png", "enemy3.png",
                                       "test3.png", "player.png", "player_sword.png",
                                       "hp.png", "ts2.png"] -- TMP!

    randInit <- randomIO

    font <- FTGL.createTextureFont "verdana.ttf"
    FTGL.setFontFaceSize font 5 5

    surface <- emptySprite viewWidth viewHeight

    let gstate = GameState { gsTileMap = tm
                           , gsActors = actors
                           , gsRandom = mkStdGen randInit
                           , gsMessages = []
                           , gsCamera = undefined
                           }
        mstate = MainState { msGameState = gstate
                           , msSprites = sprs
                           , msInput = emptyInput
                           , msFont = font
                           , msSurface = surface
                           , msScreenW = screenWidth
                           , msScreenH = screenHeight
                           }
    print actors
    mainLoop mstate (0, 0)

    where actors = --addArrs 199 $
                   newEnemy (300, 50) +:
                   {-newEnemy (100, 30) +:-}
                   newPlayer (50, 100) +: IL.empty
          arr n = newArrow (0, n * 20) 0 1 DirLeft 
          addArrs 0 il = il
          addArrs n il = arr n +: addArrs (n-1) il
          tm = testMap
