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

processEvents :: GameState -> [Event] -> GameState
processEvents gs = foldl f gs
    where
        f :: GameState -> Event -> GameState
        f gs (AddActor a) = gs { gsActors = a `IL.insert` gsActors gs  }
        f gs (RemoveActor id) = gs { gsActors = id `IL.delete` gsActors gs }
        f gs (SendMessage msg) = gs { gsMessages = msg : gsMessages gs }

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
                          msgs   = gsMessages gstate

                          -- Dispatch messages from last frame, update actors
                          ustate = UpdateState { usTileMap = tm, usSelfId = undefined }
                          mup    = dispatchMessages msgs actors >>= updateActors
                          (evs, actors', random') = runAct mup random ustate

                          -- Process events
                          gstate' = gstate { gsActors = actors'
                                           , gsRandom = random'
                                           , gsMessages = [] }

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
                                 , gsRandom = mkStdGen 100
                                 , gsMessages = [] }
              mstate = MainState { msGameState = gstate
                                 , msImages = newImageMap }

          mainLoop mstate screen

    where actors = (AnyActor $ newNPC (50, 100)) `IL.insert` ((AnyActor $ newNPC (0, 0)) `IL.insert` IL.empty)
          tm = array ((0, 0), (100, 100)) [((x, y), y*100+x) | x <- [0..100], y <- [0..100]]
