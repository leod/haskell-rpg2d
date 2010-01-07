{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Actor.Player (newPlayer) where

import Data.Maybe
import Control.Monad

import Actor
import Util
import Input
import Render
import Consts
import Anim
import Consts
import TileMap
import Actor.Arrow

data Player = Player { pos :: !Point2
                     , vel :: !Point2
                     , dir :: !Direction
                     , anim :: !Anim
                     , walking :: !Bool
                     , lastShot :: !Int
                     , attacking :: !Bool
                     } deriving Show

clip :: Point2
clip = (23, 26)

offset :: Point2
offset = (4, 4)

size :: Point2
size = (12, 20)

file = "linkanim.png"

instance Actor Player where
    neededResources _ = [file]

    update self = do
        inp <- input
        tm <- tileMap

        let -- If there is input, figure out in which direction we're walking
            walkDir = if walking self && inputHasDir (dir self) inp
                          then Just (dir self)
                          else dirFromInput inp

            -- Multiplier for if we're running
            runMult = if inCtrl inp then 2 else 1

            -- Our current velocity
            vel' = maybe (0, 0) ((^* 2) . dirToVel) walkDir ^* runMult

            -- Additional stride velocity if two directions are used at the same time
            (stride, strideDir) = maybe ((0, 0), Nothing) (getStride inp) walkDir

            -- Figure out new position
            tryWalk pos dir vel =
                let pos' = pos ^+^ vel
                    (edge1, edge2) = case dir of
                                         DirLeft -> (pos', pos' ^+^ (0, 0))
                                         DirRight -> (pos' ^+^ (px size, 0), pos' ^+^ (px size, 0))
                                         DirUp -> (pos' ^+^ (0, 0), pos' ^+^ (px size, 0))
                                         DirDown -> (pos' ^+^ (0, 0), pos' ^+^ (px size, 0)) 
                    t1 = (edge1 ^+^ offset) ^/^ (tileWidth, tileHeight)
                    t2 = (edge2 ^+^ offset) ^/^ (tileWidth, tileHeight)
                in if vel == (0, 0)
                       then pos
                       else if isTileSet 1 t1 tm || isTileSet 1 t2 tm then pos else pos'

            pos' = tryWalk
                       (tryWalk (pos self) (fromJust walkDir) vel') -- First try walking into the normal direction
                       (fromJust strideDir) stride -- And then the stride direction

            -- Currently walking?
            walking' = inLArrow inp || inRArrow inp || inUArrow inp || inDArrow inp
            anim' = if walking' -- Use
                        then updateAnim (4 `div` runMult) 7 $ anim self -- Use walk animation
                        else
                            if attacking'
                                then updateAnim 2 2 $ anim self -- Use attack animation
                                else fixFrame 0

            -- Attacking
            attacking' = inSpace inp
            lastShot' = if lastShot self > 0 then lastShot self - 1 else 0
            doShoot = inSpace inp && lastShot' == 0


        {-evDebug $ "stride: [" ++ show strideDir ++ ":" ++ show stride ++ "] vel: " ++ show walkDir ++ ":" ++ show vel' ++ "]"-}
        {-when (isTileSet 1 (pos' ^/^ (tileWidth, tileHeight)) tm) $-}
            {-evDebug "coll"-}

        evMoveCamera (px pos' - viewWidth `div` 2 + px clip `div` 2,
                      py pos' - viewHeight `div` 2 + py clip `div` 2)

        myId <- selfId

        when doShoot $
            evAddActor (newArrow (pos self ^+^ spawnOffset self) myId 7 (dir self))

        return self { pos = pos'
                    , dir = fromMaybe (dir self) walkDir
                    , walking = walking'
                    , anim = anim'
                    , attacking = attacking'
                    , lastShot = if doShoot then 20 else lastShot'
                    }
        
    {-posRect self = mkRect (pos self ^+ 4) (14, 20)-}
    posRect self = mkRect (pos self ^+^ offset) size
    render sprs self = spriteClipped (getSprite (if attacking self then undefined else file) sprs)
                                     (pos self)
                                     (px clip * (animFrame . anim) self, py clip * dirToRow (dir self)) clip
     
    collision (id, _) self = do
        when (attacking self) $
            evMessage id (Impact 100 $ dirToVel (dir self) ^* 4) 
        return self

newPlayer :: Point2 -> AnyActor
newPlayer p = AnyActor
    Player { pos = p
           , vel = (0, 0)
           , dir = DirLeft
           , walking = False
           , anim = fixFrame 0 
           , lastShot = 0
           , attacking = False
           }

spawnOffset :: Player -> Point2
spawnOffset Player { dir=DirLeft } = (-6, 8)
spawnOffset Player { dir=DirRight } = (10, 8)
spawnOffset Player { dir=DirUp } = (5, 8)
spawnOffset Player { dir=DirDown } = (5, -8)

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

getStride :: Input -> Direction -> (Point2, Maybe Direction)
getStride Input { inUArrow = True } DirLeft  = ((0, 1), Just DirUp)
getStride Input { inDArrow = True } DirLeft  = ((0, -1), Just DirDown)
getStride Input { inUArrow = True } DirRight = ((0, 1), Just DirUp)
getStride Input { inDArrow = True } DirRight = ((0, -1), Just DirDown)
getStride Input { inLArrow = True } DirUp    = ((-1, 0), Just DirLeft)
getStride Input { inRArrow = True } DirUp    = ((1, 0), Just DirRight)
getStride Input { inLArrow = True } DirDown  = ((-1, 0), Just DirLeft)
getStride Input { inRArrow = True } DirDown  = ((1, 0), Just DirRight)
getStride _ _ = ((0, 0), Nothing)

dirToRow DirDown = 3
dirToRow DirLeft = 0
dirToRow DirRight = 2
dirToRow DirUp = 1
