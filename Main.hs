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

{-processEvents :: GameState -> [Event] -> GameState-}
{-processEvents gs = foldl f gs-}
    {-where-}
        {-f :: GameState -> Event -> GameState-}
        {-f gs (AddActor a) = gs { gsActors = a `IL.insert` gsActors gs  }-}
        {-f gs (RemoveActor id) = gs { gsActors = id `IL.delete` gsActors gs }-}
        {-f gs (SendMessage msg) = gs { gsMessages = msg : gsMessages gs }-}

processActorEvents :: ActorList -> [Event] -> ActorList
processActorEvents as = foldl f as
   where
    f :: ActorList -> Event -> ActorList
    f as (AddActor a) = a `IL.insert` as
    f as (RemoveActor id) = id `IL.delete` as 
    f as _ = as
    
data MainState = MainState { msGameState :: GameState
                           , msSprites :: SpriteMap
                           }

mainLoop :: MainState -> (Word32, Int) -> IO ()
mainLoop mstate (time, frames) = 
    let sprites = msSprites mstate
        gstate  = msGameState mstate
     
        actors = gsActors gstate
        random = gsRandom gstate
        tm     = gsTileMap gstate
        msgs   = gsMessages gstate
      
        -- Dispatch messages from last frame, update actors
        ustate = UpdateState { usTileMap = tm
                             , usSelfId = error "self id not set"
                             }
        mup    = dispatchMessages msgs actors >>= updateActors
        (evs, actors', !random') = runAct mup random ustate
      
        -- Process events
        gstate' = gstate { gsActors = processActorEvents actors' evs
                         , gsRandom = random'
                         , gsMessages = []
                         }

    in do
        GL.clear [GL.ColorBuffer]

        sprites'' <- addSprite "tilemap.bmp" sprites
        sprites' <- addSprite "npc.bmp" sprites''

        GL.matrixMode $= GL.Projection
        GL.loadIdentity
        GL.ortho 0 640 480 0 0 1

        GL.matrixMode $= GL.Modelview 0
        GL.loadIdentity

        renderTileMap tm sprites'
        renderActors actors' sprites'

        {-let spr = getSprite "tilemap.bmp" sprites'-}
        {-sprite spr (0, 0)-}
      
        glSwapBuffers
        ev <- pollEvent

        tB <- getTicks

        when (tB - time > 1000) $
            print frames

        let frmCtr = if (tB - time) > 1000 then (tB, 0)
                                           else (time, frames+1)

        {-delay 10-}
        {-print $ if tB - time == 0 then 0 else 1000 / fromIntegral (tB - tA)-}
      
        case ev of
            SDL.Quit -> return ()
            otherwise -> mainLoop mstate { msSprites = sprites'
                                         , msGameState = gstate'
                                         } frmCtr

main = do SDL.init [SDL.InitEverything]
          SDL.setVideoMode 640 480 32 [SDL.OpenGL]

          GL.clearColor $= (Color4 0 1.0 0 0)
          GL.viewport $= (Position 0 0, Size 640 480)

          let gstate = GameState { gsTileMap = tm
                                 , gsActors = actors
                                 , gsRandom = mkStdGen 100
                                 , gsMessages = [] }
              mstate = MainState { msGameState = gstate
                                 , msSprites = newSpriteMap }
          mainLoop mstate (0, 0)

    where actors = {-(AnyActor $ newNPC (50, 100)) `IL.insert` ((AnyActor $ newNPC (50, 100)) `IL.insert`-} ((AnyActor $ newNPC (100, 300)) `IL.insert` IL.empty)
          tm = array ((0, 0), (40, 30)) [((x, y), y*10+x) | x <- [0..40], y <- [0..30]]
