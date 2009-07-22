{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Array
import Control.Monad
import Control.Monad.Writer
import System.Random
import Data.Map as Map
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
import Player
import Consts
import Enemy
import Render
import Input
import Util 
import qualified IdentityList as IL
import IdentityList ((+:))
import Arrow

clampCamera :: GameState -> Point2 -> Point2
clampCamera gs = min maxX . max 0 *** min maxY . max 0 
    where tm = gsTileMap gs
          maxX = mapPixelWidth  tm - viewWidth
          maxY = mapPixelHeight tm - viewHeight

processEvents :: GameState -> [Event] -> GameState
processEvents = foldl' f
    where f :: GameState -> Event -> GameState
          f gs (AddActor a) = gs { gsActors = a +: gsActors gs  }
          f gs (RemoveActor id) = gs { gsActors =id `IL.delete` gsActors gs }
          f gs (MoveCamera p) = gs { gsCamera = clampCamera gs p }
          f gs _ = gs

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

updateGS :: GameState -> Input -> GameState
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
    in gs'

renderMS :: MainState -> IO ()
renderMS MainState { msGameState = gs
                   , msSprites = sprs
                   , msFont = font
                   , msSurface = surface
                   , msScreenW = screenW
                   , msScreenH = screenH
                   } = do
    GL.clear [GL.ColorBuffer]

    GL.blend $= GL.Enabled
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GL.ortho 0 (fromIntegral viewWidth) 0 (fromIntegral viewHeight) 0 128

    GL.matrixMode $= GL.Modelview 0
    GL.loadIdentity
    
    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GL.ortho 0 (fromIntegral viewWidth) 0 (fromIntegral viewHeight) 0 128

    GL.matrixMode $= GL.Modelview 0

    GL.preservingMatrix $ renderToSprite surface $ do
        GL.translate $ Vector3 (fromIntegral . negate . px $ gsCamera gs)
                               (fromIntegral . negate . py $ gsCamera gs)
                               (0 :: Double)

        renderTileMap (gsTileMap gs) sprs
        renderActors (gsActors gs) sprs

        GL.matrixMode $= GL.Modelview 0
        GL.loadIdentity

        sprite (getSprite "hp.png" sprs) (10, viewHeight - 20)

        
    GL.color $ GL.Color3 0 0 (0::Double)
    --FTGL.renderFont font "hello world" FTGL.All
    GL.color $ GL.Color3 1 1 (1::Double)

    GL.clear [GL.ColorBuffer]

    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GL.ortho 0 800 600 0 0 128

    GL.matrixMode $= GL.Modelview 0
    GL.loadIdentity
    GL.scale ((fromIntegral screenW) / (fromIntegral viewWidth))
             ((fromIntegral screenH) / (fromIntegral viewHeight))
             (1::Double)
    
    sprite surface (0, 0)

    SDL.glSwapBuffers

mainLoop :: MainState -> (Word32, Int) -> IO ()
mainLoop mstate (time, frames) = do
    sdlEvents <- pollEvents

    -- Update
    let input = updateInput (msInput mstate) sdlEvents

        gstate = updateGS (msGameState mstate) input
        mstate' = mstate { msGameState = gstate
                         , msInput = input } 

        sprites = msSprites mstate'
        actors  = gsActors gstate
        random  = gsRandom gstate
        tm      = gsTileMap gstate
        msgs    = gsMessages gstate

    -- Render
    renderMS mstate'

    time' <- SDL.getTicks

    when (time' - time > 1000) $
        putStrLn $ "FPS: " ++ show frames

    let frmCtr = if (time' - time) > 1000
                     then (time', 0)
                     else (time, frames+1)

    {-print $ length (IL.toList actors)-}
    SDL.delay 20

    if inQuit input
        then return ()
        else mainLoop mstate' frmCtr

main = do
    SDL.init [SDL.InitEverything]
    SDL.setVideoMode 800 600 32 [SDL.OpenGL]

    GL.clearColor $= GL.Color4 0 0 0 0
    GL.viewport $= (GL.Position 0 0, GL.Size 800 600)

    sprs <- newSpriteMap `addSprites` ["test2.png", "npc.bmp", "linkanim.png", "test.png", "enemy.png",
                                       "tileset.png", "ts.png", "arrow.png", "enemy2.png", "enemy3.png",
                                       "test3.png", "player.png", "player_sword.png",
                                       "hp.png"] -- TMP!

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
                           , msScreenW = 800
                           , msScreenH = 600
                           }
    print actors
    mainLoop mstate (0, 0)

    where actors = --addArrs 199 $
                   newEnemy (300, 100) +:
                   newEnemy (100, 30) +:
                   newPlayer (100, 100) +: IL.empty
          arr n = newArrow (0, n * 20) 0 1 DirLeft 
          addArrs 0 il = il
          addArrs n il = arr n +: addArrs (n-1) il
          tm = testMap
