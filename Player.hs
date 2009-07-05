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
                     } deriving Show

clip :: Point2
clip = (23, 25)

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
            vel' = fromMaybe (0, 0) $ dirToVel `liftM` dir'
            pos' = pos self ^+ vel'

        evMoveCamera (-px pos' + viewWidth `div` 2, -py pos' + viewHeight `div` 2)

        return self { pos = pos'
                    , dir = fromMaybe (dir self) dir' }
        
    posRect self = Rect 0 0 0 0
    render self sprs = spriteClipped (getSprite file sprs) (pos self) (0, py clip * dirToRow (dir self)) clip

newPlayer :: Point2 -> AnyActor
newPlayer p = AnyActor $ Player { pos = p, vel = (0, 0), dir = DirLeft }
