module NPC (NPC, newNPC) where

import Control.Monad
import Control.Monad.Random
import Graphics.UI.SDL

import Actor
import Util
import Resource

data NPC = NPC {
      pos :: Point2
    , vel :: Point2
} deriving Show

instance Actor NPC where
    update self = do let pos' = pos self ^+ vel self 
                         vel' = if px pos' > 640 then (-1, 0)
                                else if px pos' < 0 then (1, 0)
                                else vel self
                     return $ self { pos = pos'
                                   , vel = vel' }

    render self = drawImage "npc.bmp" (pos self)

newNPC :: Point2 -> NPC
newNPC p = NPC { pos = p, vel = (1, 0) }
