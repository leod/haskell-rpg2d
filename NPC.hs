module NPC (NPC, newNPC) where

import Control.Monad
import Control.Monad.Random
import Control.Monad.Reader

import Actor
import Util
import Render

data NPC = NPC { pos :: Point2
               , vel :: Point2
               } deriving Show

instance Actor NPC where
    neededResources _ = ["npc.bmp"]

    update self = do let pos' = pos self ^+ vel self 
                         vel' = if px pos' > 640 then (-1, 0)
                                else if px pos' < 0 then (1, 0)
                                else vel self

                     {-selfId <- asks usSelfId-}
                     {-when (px pos' > 640) $-}
                        {-evMessage selfId $ Impact 200 (1, 2)-}

                     return $ self { pos = pos'
                                   , vel = vel'
                                   }

    render self sprs = sprite (getSprite "npc.bmp" sprs) (pos self)

    message self (Impact dmg vel) = evRemoveSelf >> return self

    posRect NPC { pos = (x, y) } = Rect x y 50 50

    collision self (oid, _) = evRemoveSelf >> evMessage oid (Impact 200 (1,2)) >> return self

newNPC :: Point2 -> Point2 -> NPC
newNPC p v = NPC { pos = p, vel = v }
