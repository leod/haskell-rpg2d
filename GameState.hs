module GameState (
    GameState(GameState, gsTileMap, gsActors, gsRandom),
    processEvents
    ) where

import Actor
import Util
import TileMap
import qualified IdentityList as IL

data GameState = GameState {
      gsTileMap :: TileMap
    , gsActors :: ActorList
    , gsRandom :: DefGen
} deriving Show

processEvents :: GameState -> [Event] -> GameState
processEvents gs = foldl f gs
    where
        f :: GameState -> Event -> GameState
        f gs (AddActor a) = gs { gsActors = a `IL.insert` gsActors gs  }
        f gs (RemoveActor id) = gs { gsActors = id `IL.delete` gsActors gs }
