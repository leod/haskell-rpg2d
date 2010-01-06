{-# LANGUAGE TemplateHaskell #-}
module Actor.Enemy (newEnemy) where

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
import TileMap

data Enemy = Enemy { pos_ :: !Point2
                   , dir_ :: !Direction
                   , anim_ :: !Anim
                   , extraVel_ :: !Point2
                   , extraVelTime_ :: !Int
                   } deriving Show

$(deriveAccessors ''Enemy)

clip :: (Int, Int)
clip = (32, 48)

dirToRow DirDown = 0
dirToRow DirLeft = 1
dirToRow DirRight = 2
dirToRow DirUp = 3

vel = dirToVel . dir_

instance Actor Enemy where
    neededResources _ = ["enemy2.png"]

    -- Some experimentation with using State+Accessors
    update = execStateT $ do
        anim %: updateAnim 6 3

        v <- liftM dirToVel $ get dir
        ve <- get extraVel
        pos %: (^+^ (v ^+^ ve))

        pos' <- get pos
        dir' <- get dir

        when (px pos' <= 0) $ do
            dir %= DirRight
            pos %= (0, py pos')

        when (py pos' <= 0) $ do
            dir %= DirUp
            pos %= (px pos', 0)

        tm <- tileMap
        when ((px pos' + 20) >= tmPixelWidth tm) $ do
            pos %= (tmPixelWidth tm - 20, py pos')
            dir %= DirLeft
        
        when ((py pos' + 20) >= tmPixelHeight tm) $ do
            pos %= (px pos', tmPixelHeight tm - 20)
            dir %= DirDown

        velTime <- get extraVelTime
        if velTime == 0
            then extraVel %= (0, 0)
            else extraVelTime %: subtract 1

    render sprs self = spriteClipped (getSprite "enemy3.png" sprs)
                                     (pos_ self)
                                     (px clip * (animFrame . anim_) self, py clip * dirToRow (dir_ self))
                                     clip
    
    posRect self = mkRect (pos_ self ^+^ (5, 12)) (20, 20)

    message (Impact dmg imvel) self =
        return self { extraVel_ = imvel ^* 3 ^+^ vel self
                    , extraVelTime_ = 2
                    , dir_ = (oppositeDir . dirFromVec) imvel
                    }
    message _ self = return self

newEnemy :: Point2 -> AnyActor
newEnemy p = AnyActor Enemy { pos_ = p
                            , dir_ = DirDown
                            , anim_ = fixFrame 0
                            , extraVel_ = (0, 0)
                            , extraVelTime_ = 0
                            }
