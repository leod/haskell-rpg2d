{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Array
import Control.Monad
import System.Random
import Data.Map as Map
import Graphics.UI.SDL as SDL hiding (Event)
import Graphics.Rendering.OpenGL as GL
import Foreign (Word32)

import Actor
import TileMap
import GameState
import NPC
import Render
import qualified IdentityList as IL

processEvents :: GameState -> [Event] -> GameState
processEvents = foldl f
    where
        f :: GameState -> Event -> GameState
        f gs (AddActor a) = gs { gsActors = a `IL.insert` gsActors gs  }
        f gs (RemoveActor id) = gs { gsActors = id `IL.delete` gsActors gs }
        f gs (SendMessage msg) = gs { gsMessages = msg : gsMessages gs }

data MainState = MainState { msGameState :: GameState
                           , msSprites :: SpriteMap
                           }

updateGS :: GameState -> GameState
updateGS gs =
    let actors = gsActors gs
        random = gsRandom gs
        tm     = gsTileMap gs
        msgs   = gsMessages gs

        -- Dispatch messages from last frame, update actors
        ustate = UpdateState { usTileMap = tm
                             , usSelfId = error "self id not set"
                             }
        act = dispatchMessages actors msgs >>= updateActors >>= collisions
        (actors', !random', evs) = runAct act random ustate
      
        -- Process events
        gs' = gs { gsActors = actors', gsRandom = random' }
                  `processEvents` evs
    in gs'

renderMS :: MainState -> IO ()
renderMS (MainState { msGameState = gs, msSprites = sprs }) = do
    GL.clear [GL.ColorBuffer]

    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GL.ortho 0 640 480 0 0 128

    GL.matrixMode $= GL.Modelview 0
    GL.loadIdentity

    renderTileMap (gsTileMap gs) $ sprs
    renderActors (gsActors gs) $ sprs

    SDL.glSwapBuffers

mainLoop :: MainState -> (Word32, Int) -> IO ()
mainLoop mstate (time, frames) = 
    let sprites = msSprites mstate

        gstate = updateGS $ msGameState mstate
        mstate' = mstate { msGameState = gstate }

        actors = gsActors gstate
        random = gsRandom gstate
        tm     = gsTileMap gstate
        msgs   = gsMessages gstate
    in do
        renderMS mstate'

        time' <- getTicks

        when (time' - time > 1000) $
            print frames

        let frmCtr = if (time' - time) > 1000
                     then (time', 0)
                     else (time, frames+1)
        {-delay 10-}
      
        ev <- SDL.pollEvent
        case ev of
            SDL.Quit -> return ()
            otherwise -> mainLoop mstate' frmCtr

main = do
    SDL.init [SDL.InitEverything]
    SDL.setVideoMode 640 480 32 [SDL.OpenGL]

    GL.clearColor $= (Color4 1 1 1 0)
    GL.viewport $= (Position 0 0, Size 640 480)

    sprs <- (newSpriteMap `addSprite` "test2.png") >>= (`addSprite` "npc.bmp")

    let gstate = GameState { gsTileMap = tm
                           , gsActors = actors
                           , gsRandom = mkStdGen 100
                           , gsMessages = []
                           }
        mstate = MainState { msGameState = gstate
                           , msSprites = sprs
                           }
    mainLoop mstate (0, 0)

    where actors = (AnyActor $ newNPC (480, 280) (-1, 0)) `IL.insert` ((AnyActor $ newNPC (100, 300) (1, 0)) `IL.insert` IL.empty)
          tm = array ((0, 0), (40, 30)) [((x, y), y*10+x) | x <- [0..40], y <- [0..30]]
