module NPC (newNPC) where

import System.IO.Unsafe
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

    collision self (oid, _) = do
        evRemoveSelf
        evMessage oid (Impact 200 (1,2)) 

        nx <- getRandomR (100, 200)
        ny <- getRandomR (100, 200)
        nvx <- getRandomR (-1, 1)

        evAddActor $ newNPC (nx, ny) (nvx, 0)
                                 
        return self

newNPC :: Point2 -> Point2 -> AnyActor
newNPC p v = AnyActor NPC { pos = p, vel = v }
