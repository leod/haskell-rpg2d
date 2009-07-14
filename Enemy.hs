module Enemy (newEnemy) where

import System.IO.Unsafe
import Control.Monad
import Control.Monad.Random
import Control.Monad.Reader

import Actor
import Util
import Render
import Anim

data Enemy = Enemy { pos :: !Point2
                   , dir :: !Direction
                   , anim :: !Anim
                   , extraVel :: !Point2
                   , extraVelTime :: !Int
                   } deriving Show

clip :: (Int, Int)
clip = (37, 40)

dirToColumn DirDown = 0
dirToColumn DirLeft = 1
dirToColumn DirUp = 2
dirToColumn DirRight = 3

vel self = dirToVel $ dir self

instance Actor Enemy where
    neededResources _ = ["enemy.png"]

    update self = do
        let anim' = updateAnim 8 2 $ anim self
            pos' = pos self ^+^ vel self ^+^ extraVel self 

        return $ self { pos = pos'
                      , anim = anim'
                      , extraVel = if extraVelTime self == 0 then (0, 0) else extraVel self
                      , extraVelTime = if extraVelTime self > 0 then extraVelTime self - 1 else extraVelTime self
                      }

    render sprs self = spriteClipped (getSprite "enemy.png" sprs)
                                     (pos self)
                                     (px clip * dirToColumn (dir self), py clip * (animFrame . anim) self)
                                     clip
    
    posRect self = mkRect (pos self ^+ 7) (20, 20)

    message (Impact dmg imvel) self = {-evRemoveSelf >>-} return self { extraVel = imvel ^* 3 ^+^ vel self, extraVelTime = 2 }
    message _ self = return self

newEnemy :: Point2 -> AnyActor
newEnemy p = AnyActor Enemy { pos = p
                            , dir = DirDown
                            , anim = fixFrame 0
                            , extraVel = (0, 0)
                            , extraVelTime = 0
                            }
