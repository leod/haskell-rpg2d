{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Player (newPlayer) where

import Data.Maybe
import Control.Monad

import Actor
import Util
import Input
import Render
import Consts
import Anim
import Arrow

data Player = Player { pos :: !Point2
                     , vel :: !Point2
                     , dir :: !Direction
                     , anim :: !Anim
                     , walking :: !Bool
                     , lastShot :: !Int
                     } deriving Show

clip :: Point2
clip = (23, 26)

file = "linkanim.png"

instance Actor Player where
    neededResources _ = [file]

    update self = do
        inp <- input

        let walkDir = if walking self && inputHasDir (dir self) inp
                          then Just (dir self)
                          else dirFromInput inp
            vel' = maybe (0, 0) ((^* 3) . dirToVel) walkDir
            pos' = pos self ^+^ vel' ^+^ maybe (0, 0) (strideVel inp) walkDir 
            walking' = inLArrow inp || inRArrow inp || inUArrow inp || inDArrow inp
            anim' = if walking' then updateAnim 4 7 $ anim self else fixFrame 0

            lastShot' = if lastShot self > 0 then lastShot self - 1 else 0
            doShoot = inSpace inp && lastShot' == 0

        evMoveCamera (-px pos' + viewWidth `div` 2 - px clip `div` 2,
                      -py pos' + viewHeight `div` 2 - py clip `div` 2)

        myId <- selfId

        when doShoot $
            evAddActor (newArrow (pos self ^+^ spawnOffset self) myId 7 (dir self))

        return self { pos = pos'
                    , dir = fromMaybe (dir self) walkDir
                    , walking = walking'
                    , anim = anim'
                    , lastShot = if doShoot then 20 else lastShot'
                    }
        
    posRect self = mkRect (pos self ^+ 4) (12, 17)
    render sprs self = spriteClipped (getSprite file sprs)
                                     (pos self)
                                     (px clip * (animFrame . anim) self, py clip * dirToRow (dir self)) clip

newPlayer :: Point2 -> AnyActor
newPlayer p = AnyActor
    Player { pos = p
           , vel = (0, 0)
           , dir = DirLeft
           , walking = False
           , anim = fixFrame 0 
           , lastShot = 0
           }

spawnOffset :: Player -> Point2
spawnOffset Player { dir=DirLeft } = (-6, 8)
spawnOffset Player { dir=DirRight } = (10, 8)
spawnOffset Player { dir=DirUp } = (5, -8)
spawnOffset Player { dir=DirDown } = (5, 8)

inputHasDir :: Direction -> Input -> Bool
inputHasDir DirLeft = inLArrow
inputHasDir DirRight = inRArrow
inputHasDir DirUp = inUArrow
inputHasDir DirDown = inDArrow

dirFromInput :: Input -> Maybe Direction
dirFromInput Input { inLArrow = True } = Just DirLeft
dirFromInput Input { inRArrow = True } = Just DirRight
dirFromInput Input { inUArrow = True } = Just DirUp
dirFromInput Input { inDArrow = True } = Just DirDown
dirFromInput _                         = Nothing

strideVel :: Input -> Direction -> Point2
strideVel Input { inUArrow = True } DirLeft  = (0, -1)
strideVel Input { inDArrow = True } DirLeft  = (0, 1)
strideVel Input { inUArrow = True } DirRight = (0, -1)
strideVel Input { inDArrow = True } DirRight = (0, 1)
strideVel Input { inLArrow = True } DirUp    = (-1, 0)
strideVel Input { inRArrow = True } DirUp    = (1, 0)
strideVel Input { inLArrow = True } DirDown  = (-1, 0)
strideVel Input { inRArrow = True } DirDown  = (1, 0)
strideVel _ _ = (0, 0)

dirToRow DirLeft = 0
dirToRow DirUp = 1
dirToRow DirRight = 2
dirToRow DirDown = 3
