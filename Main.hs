module Main where

import Data.Array
import System.Random
import Graphics.UI.SDL as SDL hiding (Event)

import Actor
import TileMap
import GameState
import NPC
import Resource
import qualified IdentityList as IL

data MainState = MainState {
      msGameState :: GameState
    , msImages :: ImageMap
}

mainLoop :: MainState -> Surface -> IO ()
mainLoop mstate sur = let images = msImages mstate
                          gstate = msGameState mstate

                          actors = gsActors gstate
                          random = gsRandom gstate
                          tm     = gsTileMap gstate

                          -- Update actors
                          ustate = UpdateState { usTileMap = tm, usSelfId = undefined }
                          (evs, actors', random') = runAct (updateActors actors) random ustate

                          -- Process events
                          gstate' = gstate { gsActors = actors'
                                           , gsRandom = random' }

                          gstate'' = processEvents gstate' evs

                      in do (_, images') <- runRenderer (renderActors actors sur) images
                        
                            SDL.flip sur
                            fillRect sur Nothing (Pixel 0)

                            ev <- pollEvent

                            case ev of
                                SDL.Quit -> return ()
                                otherwise -> mainLoop mstate { msImages = images'
                                                             , msGameState = gstate'' } sur

main = do SDL.init [SDL.InitEverything]
          SDL.setVideoMode 640 480 32 []

          screen <- getVideoSurface

          let gstate = GameState { gsTileMap = tm
                                 , gsActors = actors
                                 , gsRandom = mkStdGen 100 }
              mstate = MainState { msGameState = gstate
                                 , msImages = newImageMap }

          mainLoop mstate screen

    where --actors = [ActorRec 0 (AnyActor $ newNPC (1, 2)), ActorRec 1 (AnyActor $ newNPC (1, 2))]
          actors = (AnyActor $ newNPC (1, 2)) `IL.insert` IL.empty
          tm = array ((0, 0), (100, 100)) [((x, y), y*100+x) | x <- [0..100], y <- [0..100]]
