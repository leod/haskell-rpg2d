module GameState (
    GameState(GameState, gsTileMap, gsActors, gsRandom, gsMessages)
    ) where

import Actor
import Util
import TileMap
import qualified IdentityList as IL

data GameState = GameState { gsTileMap :: TileMap
                           , gsActors :: ActorList
                           , gsRandom :: DefGen
                           , gsMessages :: [MessageRec]
                           } deriving Show
