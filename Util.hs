module Util
    ( DefGen
    , Point2
    , Rect(Rect), rectIntersect
    , (^+), (^*)
    , px, py
    , Direction(..)
    , dirFromBools
    , dirToVel
    ) where

import System.Random

type DefGen = StdGen

type Point2 = (Int, Int)
data Rect = Rect Int Int Int Int

rectIntersect :: Rect -> Rect -> Bool
rectIntersect (Rect x1 y1 w1 h1) (Rect x2 y2 w2 h2) =
    not $ x1 > x2 + w2 || x1 + w1 < x2 || y1 > y2 + h2 || y1 + h1 < y2

infix 6 ^+

(^+) :: Point2 -> Point2 -> Point2
(x1, y1) ^+ (x2, y2) = (x1 + x2, y1 + y2)

(^*) :: Point2 -> Point2 -> Point2
(x1, y1) ^* (x2, y2) = (x1 * x2, y1 * y2)

px :: Point2 -> Int
px (x, _) = x

py :: Point2 -> Int
py (_, y) = y

dirFromBools :: Bool -> Bool -> Bool -> Bool -> Maybe Direction
dirFromBools True _ _ _ = Just DirLeft
dirFromBools _ True _ _ = Just DirRight
dirFromBools _ _ True _ = Just DirUp
dirFromBools _ _ _ True = Just DirDown
dirFromBools _ _ _ _    = Nothing

data Direction = DirLeft | DirRight | DirUp | DirDown 
    deriving Show

dirToVel :: Direction -> Point2
dirToVel DirLeft = (-1, 0)
dirToVel DirRight = (1, 0)
dirToVel DirUp = (0, -1)
dirToVel DirDown = (0, 1)
