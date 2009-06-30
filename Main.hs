module Main where

import Data.Array
import System.Random
import Graphics.UI.SDL as SDL hiding (Event)

import Actor
import TileMap
import GameState
import NPC
import Resource

quitHandler :: IO ()
quitHandler = do e <- SDL.waitEvent
                 case e of
                    SDL.Quit -> return ()
                    otherwise -> quitHandler
                     
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

                          ustate = UpdateState { usTileMap = tm, usSelfId = undefined }
                          (evs, actors', random') = runAct (updateActors actors) random ustate

                          gstate' = gstate { gsActors = actors'
                                           , gsRandom = random' }
                      in do (_, images') <- runRenderer (renderActors actors sur) images
                        
                            SDL.flip sur
                            fillRect sur Nothing (Pixel 0)

                            ev <- pollEvent

                            case ev of
                                SDL.Quit -> return ()
                                otherwise -> mainLoop mstate { msImages = images'
                                                             , msGameState = gstate' } sur

main = do SDL.init [SDL.InitEverything]
          SDL.setVideoMode 640 480 32 []

          screen <- getVideoSurface

          let gstate = GameState { gsTileMap = tm
                                 , gsActors = actors
                                 , gsActorCounter = 2
                                 , gsRandom = mkStdGen 100 }
              mstate = MainState { msGameState = gstate
                                 , msImages = newImageMap }

          mainLoop mstate screen

    where actors = [ActorRec 0 (AnyActor $ newNPC (1, 2)), ActorRec 1 (AnyActor $ newNPC (1, 2))]
          tm = array ((0, 0), (100, 100)) [((x, y), y*100+x) | x <- [0..100], y <- [0..100]]
