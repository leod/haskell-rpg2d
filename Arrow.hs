{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Arrow (newArrow) where

import Control.Monad

import Actor
import Util
import Render

dirToRow DirLeft = 0
dirToRow DirUp = 1
dirToRow DirRight = 2
dirToRow DirDown = 3

data Arrow = Arrow { pos :: Point2
                   , owner :: ActorId
                   , speed :: Int
                   , dir :: Direction
                   } deriving Show

vel Arrow { dir, speed } = dirToVel dir ^* speed

instance Actor Arrow where
    update self = let pos' = pos self ^+^ vel self
                  in return self { pos = pos' }
    render sprs self = spriteClipped (getSprite "arrow.png" sprs)
                                     (pos self)
                                     (0, dirToRow (dir self) * 15)
                                     (15, 15)
    posRect Arrow { pos, dir } = mkRect pos $ size dir
        where
            size DirLeft = (15, 5)
            size DirRight = size DirLeft
            size DirUp = (5, 15)
            size DirDown = size DirUp

    collision (id, actor) self = do
        when (id /= owner self) $ do
            evMessage id (Impact 100 $ vel self)
            evRemoveSelf

        return self
            

newArrow :: Point2 -> ActorId -> Int -> Direction -> AnyActor
newArrow pos owner speed dir = AnyActor Arrow { .. }
