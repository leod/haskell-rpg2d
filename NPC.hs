module NPC (NPC, newNPC) where

import Control.Monad
import Control.Monad.Random
import Control.Monad.Reader
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

                     selfId <- asks usSelfId
                     when (px pos' > 640) $
                        evMessage selfId $ Impact 200 (1, 2)

                     return $ self { pos = pos'
                                   , vel = vel' }

    render self = drawImage "npc.bmp" (pos self)

    message self (Impact dmg vel) = evRemoveSelf >> return self

newNPC :: Point2 -> NPC
newNPC p = NPC { pos = p, vel = (1, 0) }
