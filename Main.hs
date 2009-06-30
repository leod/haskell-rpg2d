module Main where

import Data.Array
import System.Random
import qualified Graphics.UI.SDL as SDL

import Actor
import TileMap
import GameState
import NPC

quitHandler :: IO ()
quitHandler = do e <- SDL.waitEvent
                 case e of
                    SDL.Quit -> return ()
                    otherwise -> quitHandler
                     
main = do print actors

          let (evs, actors', g) = runAct (mkStdGen 100) (UpdateState tm undefined) (updateActors actors)
          {-print actors'-}

          let gs = GameState { gsTileMap = tm, gsActors = actors', gsActorCounter = 0, gsRandom = mkStdGen 10 }
          let gs' = processEvents gs evs
          print $ gsActors gs'

          {-print gs-}
          {-renderActors $ gsActors gs'-}

          SDL.init [SDL.InitEverything]
          SDL.setVideoMode 640 480 32 []

          quitHandler

          {-let (evs, actors'', g') = runAct g (UpdateState tm undefined) (updateActors actors')-}
          {-print actors''-}
          {-print evs-}

    where actors = [ActorRec 0 (AnyActor $ newNPC (1, 2)), ActorRec 1 (AnyActor $ newNPC (1, 2))]
          tm = array ((0, 0), (100, 100)) [((x, y), y*100+x) | x <- [0..100], y <- [0..100]]
