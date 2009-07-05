module Anim (Anim, updateAnim, fixFrame, animFrame) where

data Anim = Anim { counter :: Int
                 , frame :: Int
                 } deriving Show

updateAnim :: Int -> Int -> Anim -> Anim
updateAnim frameTime endFrame anim =
    let counter' = if counter anim == frameTime
                     then 0
                     else counter anim + 1
        frame' = if counter' == frameTime
                  then if frame anim == endFrame
                        then 0
                        else frame anim + 1
                  else frame anim
    in anim { counter = counter'
            , frame = frame'
            }

fixFrame :: Int -> Anim
fixFrame f = Anim { counter = 0
                  , frame = f }

animFrame :: Anim -> Int
animFrame = frame
