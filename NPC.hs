module NPC (NPC, newNPC) where

import Control.Monad
import Control.Monad.Random

import Actor
import Util

data NPC = NPC {
      pos :: Point2
} deriving Show

instance Actor NPC where
    update self = do x' <- getRandomR (99, 200)
                     y' <- getRandomR (100, 200)

                     when (x' < 150) $
                        evAddActor NPC { pos = (x' + 10, y' - 10) }

                     return $ self { pos = (x', y') }

    render self = undefined

newNPC :: Point2 -> NPC
newNPC p = NPC { pos = p }
