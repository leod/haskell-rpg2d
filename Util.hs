module Util (DefGen, Point2, (^+), px, py) where

import System.Random

type DefGen = StdGen

type Point2 = (Int, Int)

infix 6 ^+

(^+) :: Point2 -> Point2 -> Point2
(x1, y1) ^+ (x2, y2) = (x1 + x2, y1 + y2)

px :: Point2 -> Int
px (x, _) = x

py :: Point2 -> Int
py (_, y) = y
