module GameState (
    GameState(GameState, gsTileMap, gsActors, gsActorCounter, gsRandom),
    processEvents
    ) where

import Actor
import Util
import TileMap

data GameState = GameState {
      gsTileMap :: TileMap
    , gsActors :: [ActorRec]
    , gsActorCounter :: Int
    , gsRandom :: DefGen
} deriving Show

processEvents :: GameState -> [Event] -> GameState
processEvents gs = foldl f gs
    where
        f :: GameState -> Event -> GameState
        f gs (AddActor a) = let counter = gsActorCounter gs
                                rec = ActorRec counter a
                            in gs { gsActors = rec : (gsActors gs)
                                  , gsActorCounter = counter + 1}
        f gs (RemoveActor id) = let actors = gsActors gs
                                    actors' = filter (\(ActorRec iid _) -> iid /= id) actors
                                in gs { gsActors = actors' }
        --f gs _ = gs -- Ignore other events, they are processed somewhere else
