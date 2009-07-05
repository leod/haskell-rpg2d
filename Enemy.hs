module Enemy (newEnemy) where

import System.IO.Unsafe
import Control.Monad
import Control.Monad.Random
import Control.Monad.Reader

import Actor
import Util
import Render
import Anim

data Enemy = Enemy { pos :: Point2
                   , dir :: Direction
                   , anim :: Anim
                   } deriving Show

clip :: (Int, Int)
clip = (37, 40)

dirToColumn DirDown = 0
dirToColumn DirLeft = 1
dirToColumn DirUp = 2
dirToColumn DirRight = 3

instance Actor Enemy where
    neededResources _ = ["enemy.png"]

    update self = do
        let anim' = updateAnim 15 2 $ anim self
            vel = dirToVel $ dir self
            pos' = pos self ^+ vel

        return $ self { pos = pos'
                      , anim = anim'
                      }

    render self sprs = spriteClipped (getSprite "enemy.png" sprs)
                                     (pos self)
                                     (px clip * dirToColumn (dir self), py clip * (animFrame . anim) self)
                                     clip

    posRect self = mkRect (pos self) clip

    collision self _ = evRemoveSelf >> return self

newEnemy :: Point2 -> AnyActor
newEnemy p = AnyActor $ Enemy { pos = p
                              , dir = DirDown
                              , anim = fixFrame 0
                              }
