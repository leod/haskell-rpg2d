module GameState (
    GameState(..)
    ) where

import Actor
import Util
import TileMap
import qualified IdentityList as IL

data GameState = GameState { gsTileMap :: TileMap
                           , gsActors :: ActorList
                           , gsRandom :: DefGen
                           , gsMessages :: [MessageRec]
                           , gsCamera :: Point2
                           } deriving (Show)
