{-# LANGUAGE TemplateHaskell #-}
module Enemy (newEnemy) where

import System.IO.Unsafe
import Control.Monad
import Control.Monad.Random
import Control.Monad.Reader
import Debug.Trace
import Control.Monad.State hiding (get)
import Data.Accessor.Template
import Data.Accessor.Monad.MTL.State

import Actor
import Util
import Render
import Anim

data Enemy = Enemy { pos_ :: !Point2
                   , dir_ :: !Direction
                   , anim_ :: !Anim
                   , extraVel_ :: !Point2
                   , extraVelTime_ :: !Int
                   } deriving Show

$(deriveAccessors ''Enemy)

clip :: (Int, Int)
clip = (37, 40)

dirToColumn DirDown = 0
dirToColumn DirLeft = 1
dirToColumn DirUp = 2
dirToColumn DirRight = 3

vel = dirToVel . dir_

instance Actor Enemy where
    neededResources _ = ["enemy2.png"]

    -- Some experimentation with using State+Accessors
    update = execStateT $ do
        anim %: updateAnim 8 2

        v <- liftM dirToVel $ get dir
        ve <- get extraVel
        pos %: (^+^ v) . (^+^ ve) 

        pos' <- get pos
        dir' <- get dir
        when (px pos' <= 0 && dir' == DirLeft) $
            dir %= DirRight

        velTime <- get extraVelTime
        if velTime == 0
            then extraVel %= (0,0)
            else extraVelTime %: subtract 1

    render sprs self = spriteClipped (getSprite "enemy2.png" sprs)
                                     (pos_ self)
                                     (px clip * dirToColumn (dir_ self), py clip * (animFrame . anim_) self)
                                     clip
    
    posRect self = mkRect (pos_ self ^+ 7) (20, 20)

    message (Impact dmg imvel) self = do
        return self { extraVel_ = imvel ^* 3 ^+^ vel self
                    , extraVelTime_ = 2
                    , dir_ = (oppositeDir . oppositeDir . dirFromVec) imvel
                    }
    message _ self = return self

newEnemy :: Point2 -> AnyActor
newEnemy p = AnyActor Enemy { pos_ = p
                            , dir_ = DirDown
                            , anim_ = fixFrame 0
                            , extraVel_ = (0, 0)
                            , extraVelTime_ = 0
                            }
