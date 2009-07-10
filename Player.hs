module Player (newPlayer) where

import Data.Maybe
import Control.Monad

import Actor
import Util
import Input
import Render
import Consts
import Anim

data Player = Player { pos :: Point2
                     , vel :: Point2
                     , dir :: Direction
                     , anim :: Anim
                     , walking :: Bool
                     } deriving Show

clip :: Point2
clip = (23, 26)

dirToRow DirLeft = 0
dirToRow DirUp = 1
dirToRow DirRight = 2
dirToRow DirDown = 3

file = "linkanim.png"

instance Actor Player where
    neededResources _ = [file]

    update self = do
        inp <- input

        let dir' = dirFromBools (inLArrow inp) (inRArrow inp) (inUArrow inp) (inDArrow inp)
            vel' = fromMaybe (0, 0) $ ((^*(2,2)) . dirToVel) `liftM` dir'
            pos' = pos self ^+ vel'

            walking' = vel' /= (0, 0)

            anim' = if walking' then updateAnim 5 7 $ anim self else fixFrame 0

        evMoveCamera (-px pos' + viewWidth `div` 2 - px clip `div` 2,
                      -py pos' + viewHeight `div` 2 - py clip `div` 2)

        return self { pos = pos'
                    , dir = fromMaybe (dir self) dir'
                    , walking = walking'
                    , anim = anim'
                    }
        
    posRect self = mkRect (pos self) clip
    render self sprs = spriteClipped (getSprite file sprs)
                                     (pos self)
                                     (px clip * (animFrame . anim) self, py clip * dirToRow (dir self)) clip

newPlayer :: Point2 -> AnyActor
newPlayer p = AnyActor
    Player { pos = p
           , vel = (0, 0)
           , dir = DirLeft
           , walking = False
           , anim = fixFrame 0 
           }
