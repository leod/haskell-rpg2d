module Util
    ( DefGen
    , Point2
    , Rect(Rect), rectIntersect, mkRect
    , (^+), (^*)
    , px, py
    , Direction(..)
    , dirToVel
    ) where

import System.Random

type DefGen = StdGen

type Point2 = (Int, Int)
data Rect = Rect Int Int Int Int

rectIntersect :: Rect -> Rect -> Bool
rectIntersect (Rect x1 y1 w1 h1) (Rect x2 y2 w2 h2) =
    not $ x1 > x2 + w2 || x1 + w1 < x2 || y1 > y2 + h2 || y1 + h1 < y2

mkRect :: Point2 -> Point2 -> Rect
mkRect (x, y) (w, h) = Rect x y w h

infixl 6 ^+
infixl 7 ^*

(^+) :: Point2 -> Point2 -> Point2
(x1, y1) ^+ (x2, y2) = (x1 + x2, y1 + y2)

(^*) :: Point2 -> Point2 -> Point2
(x1, y1) ^* (x2, y2) = (x1 * x2, y1 * y2)

{-instance Num Point2 where-}
    {-(x1, y1) + (x2, y2) = (x1 + x2, y1 + y2)-}
    {-(x1, y1) * (x2, y2) = (x1 * x2, y1 * y2)-}
    {-(x1, y1) - (x2, y2) = (x1 * x2, y1 * y2)-}
    {-negate (x, y) = (-x, -y)-}
    {-abs (x, y) = (abs x, abs y)-}
    {-signum (x, y) = (signum x, signum y)-}
    {-fromInteger i = (i, i)-}

px :: Point2 -> Int
px = fst

py :: Point2 -> Int
py = snd

data Direction = DirLeft | DirRight | DirUp | DirDown 
    deriving (Show, Eq)

dirToVel :: Direction -> Point2
dirToVel DirLeft = (-1, 0)
dirToVel DirRight = (1, 0)
dirToVel DirUp = (0, -1)
dirToVel DirDown = (0, 1)
