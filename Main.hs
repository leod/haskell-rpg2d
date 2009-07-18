{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Array
import Control.Monad
import Control.Monad.Writer
import System.Random
import Data.Map as Map
import qualified Graphics.UI.SDL as SDL
import Graphics.Rendering.OpenGL as GL
import Foreign (Word32)
import Data.List (foldl')

import Actor
import Input
import TileMap
import GameState
import NPC
import Player
import Consts
import Enemy
import Render
import Input
import Util (px, py, Direction(..))
import qualified IdentityList as IL
import IdentityList ((+:))
import Arrow

processEvents :: GameState -> [Event] -> GameState
processEvents = foldl' f
    where f :: GameState -> Event -> GameState
          f gs (AddActor a) = gs { gsActors = a `IL.insert` gsActors gs  }
          f gs (RemoveActor id) = gs { gsActors = id `IL.delete` gsActors gs }
          f gs (MoveCamera p) = gs { gsCamera = p }
          f gs _ = gs

debugTest :: [Event] -> IO ()
debugTest = foldl' f $ return ()
    where f io (Debug str) = io >> putStrLn str
          f io _ = io

getMessages :: [Event] -> [MessageRec]
getMessages = foldl' f []
    where f :: [MessageRec] -> Event -> [MessageRec]
          f msgs (SendMessage msg) = msg : msgs
          f msgs _ = msgs

data MainState = MainState { msGameState :: GameState
                           , msSprites :: SpriteMap
                           , msInput :: Input
                           }

pollEvents :: IO [SDL.Event]
pollEvents = poll []
    where poll evs = do
              ev <- SDL.pollEvent

              case ev of
                  SDL.NoEvent -> return evs
                  _ -> poll $ ev : evs

-- TODO: IO () result is for debugging, remove later
updateGS :: GameState -> Input -> (GameState, IO ())
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
    in (gs', debugTest evs)

renderMS :: MainState -> IO ()
renderMS (MainState { msGameState = gs, msSprites = sprs }) = do
    {-print $ gsActors gs-}
    GL.clear [GL.ColorBuffer]

    GL.matrixMode $= GL.Projection
    GL.blend $= GL.Enabled
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    GL.loadIdentity
    GL.ortho 0 320 240 0 0 128

    GL.matrixMode $= GL.Modelview 0
    GL.loadIdentity
    
    GL.preservingMatrix $ do
        GL.translate $ Vector3 (fromIntegral . px $ gsCamera gs)
                               (fromIntegral . py $ gsCamera gs)
                               (0 :: Double)

        renderTileMap (gsTileMap gs) sprs
        renderActors (gsActors gs) sprs

    SDL.glSwapBuffers

mainLoop :: MainState -> (Word32, Int) -> IO ()
mainLoop mstate (time, frames) = do
    sdlEvents <- pollEvents

    -- Update
    let input = updateInput (msInput mstate) sdlEvents

        (gstate, io) = updateGS (msGameState mstate) input
        mstate' = mstate { msGameState = gstate
                         , msInput = input } 

        sprites = msSprites mstate'
        actors  = gsActors gstate
        random  = gsRandom gstate
        tm      = gsTileMap gstate
        msgs    = gsMessages gstate

    io

    -- Render
    renderMS mstate'

    time' <- SDL.getTicks

    when (time' - time > 1000) $
        print frames

    let frmCtr = if (time' - time) > 1000
                 then (time', 0)
                 else (time, frames+1)

    SDL.delay 20

    if inQuit input
        then return ()
        else mainLoop mstate' frmCtr

main = do
    SDL.init [SDL.InitEverything]
    SDL.setVideoMode 800 600 32 [SDL.OpenGL]

    GL.clearColor $= Color4 1 1 1 0
    GL.viewport $= (Position 0 0, Size 800 600)

    sprs <- newSpriteMap `addSprites` ["test2.png", "npc.bmp", "linkanim.png", "test.png", "enemy.png", "tileset.png", "ts.png", "arrow.png", "enemy2.png"] -- TMP!

    randInit <- randomIO

    let gstate = GameState { gsTileMap = tm
                           , gsActors = actors
                           , gsRandom = mkStdGen randInit
                           , gsMessages = []
                           , gsCamera = undefined
                           }
        mstate = MainState { msGameState = gstate
                           , msSprites = sprs
                           , msInput = emptyInput
                           }
    print actors
    mainLoop mstate (0, 0)

    where actors = addArrs 20 $ newEnemy (10, 10) +: newPlayer (100, 100) +: IL.empty
          arr = newArrow (0, 0) 0 1 DirLeft 
          addArrs 0 il = il
          addArrs n il = arr +: addArrs (n-1) il
          tm = array ((0, 0), (40, 30)) [((x, y), y*10+x) | x <- [0..40], y <- [0..30]]
