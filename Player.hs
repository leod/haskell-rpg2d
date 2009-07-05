module Player (newPlayer) where

import Data.Maybe
import Control.Monad

import Actor
import Util
import Input
import Render
import Consts

data Player = Player { pos :: Point2
                     , vel :: Point2
                     , dir :: Direction

                     , frame :: Int
                     , frameCtr :: Int
                     } deriving Show

clip :: Point2
clip = (23, 26)

dirToRow DirLeft = 0
dirToRow DirUp = 1
dirToRow DirRight = 2
dirToRow DirDown = 3

animTime = 15

file = "linkanim.png"

instance Actor Player where
    neededResources _ = [file]

    update self = do
        inp <- input

        let dir' = dirFromBools (inLArrow inp) (inRArrow inp) (inUArrow inp) (inDArrow inp)
            vel' = fromMaybe (0, 0) $ dirToVel `liftM` dir'
            pos' = pos self ^+ vel'

            walking = vel' /= (0, 0)

            frameCtr' = if walking then (if frameCtr self == animTime then 0 else frameCtr self + 1) else frameCtr self

            frame' = if walking then
                         if frameCtr' == animTime then
                             if frame self == 7 then 0
                             else frame self + 1
                         else frame self
                     else 0

        evMoveCamera (-px pos' + viewWidth `div` 2, -py pos' + viewHeight `div` 2)

        return self { pos = pos'
                    , dir = fromMaybe (dir self) dir'
                    , frameCtr = frameCtr'
                    , frame = frame'
                    }
        
    posRect self = Rect 0 0 0 0
    render self sprs = spriteClipped (getSprite file sprs) (pos self) (px clip * frame self, py clip * dirToRow (dir self)) clip

newPlayer :: Point2 -> AnyActor
newPlayer p = AnyActor $
    Player { pos = p
           , vel = (0, 0)
           , dir = DirLeft
           , frame = 0
           , frameCtr = 0
           }
