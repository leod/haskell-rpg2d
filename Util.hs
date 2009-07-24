module Util
    ( DefGen
    , Point2
    , Size2
    , Rect(Rect), rectIntersect, mkRect
    , rectPos, rectSize
    , rectX, rectY
    , rectW, rectH
    , rectX2, rectY2
    , (^+), (^-), (^*)
    , (^+^), (^-^), (^*^)
    , divPoint
    , px, py
    , pabs
    , Direction(..)
    , dirToVel
    , dirFromVec
    , oppositeDir
    , doFromTo
    ) where

import System.Random
import Control.Arrow ((***))

type DefGen = StdGen

type Point2 = (Int, Int)
type Size2 = Point2
data Rect = Rect Int Int Int Int
    deriving (Show, Eq)

rectIntersect :: Rect -> Rect -> Bool
rectIntersect (Rect x1 y1 w1 h1) (Rect x2 y2 w2 h2) =
    not $ x1 > x2 + w2 || x1 + w1 < x2 || y1 > y2 + h2 || y1 + h1 < y2

rectPos :: Rect -> Point2
rectPos (Rect x y _ _) = (x, y)

rectSize :: Rect -> Size2
rectSize (Rect _ _ w h) = (w, h)

rectX, rectY, rectW, rectH, rectX2, rectY2 :: Rect -> Int
rectX = px . rectPos
rectY = py . rectPos
rectW = px . rectSize
rectH = py . rectSize
rectX2 r = rectX r + rectW r
rectY2 r = rectY r + rectH r

mkRect :: Point2 -> Point2 -> Rect
mkRect (x, y) (w, h) = Rect x y w h

-- Wow, those are some awfully named operators

infixl 6 ^+^
infixl 6 ^-^
infixl 7 ^*^

(^+^) :: Point2 -> Point2 -> Point2
(x1, y1) ^+^ (x2, y2) = (x1 + x2, y1 + y2)

(^-^) :: Point2 -> Point2 -> Point2
(x1, y1) ^-^ (x2, y2) = (x1 - x2, y1 - y2)

(^*^) :: Point2 -> Point2 -> Point2
(x1, y1) ^*^ (x2, y2) = (x1 * x2, y1 * y2)

infixl 6 ^+
infixl 6 ^-
infixl 7 ^*

(^*) :: Point2 -> Int -> Point2
(x1, y1) ^* i = (x1 * i, y1 * i)

(^-) :: Point2 -> Int -> Point2
(x1, y1) ^- i = (x1 - i, y1 - i)

(^+) :: Point2 -> Int -> Point2
(x1, y1) ^+ i = (x1 + i, y1 + i)

divPoint :: Point2 -> Int -> Point2
(x, y) `divPoint` i = (x `div` i, y `div` i)

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

pabs :: Point2 -> Point2
pabs = abs *** abs

data Direction = DirLeft | DirRight | DirUp | DirDown 
    deriving (Show, Eq)

dirToVel :: Direction -> Point2
dirToVel DirLeft = (-1, 0)
dirToVel DirRight = (1, 0)
dirToVel DirUp = (0, 1)
dirToVel DirDown = (0, -1)

dirFromVec :: Point2 -> Direction
dirFromVec (x, y) | x < 0 && x < y  = DirLeft
                  | y < 0 && y <= x = DirUp
                  | x > 0 && x > y  = DirRight
                  | otherwise       = DirDown

instance Random Direction where
    randomR _ = error "don't use randomR for Direction"
    random g = let (i, g') = randomR (0, 3) g
                   d = case i :: Int of
                           0 -> DirLeft
                           1 -> DirRight
                           2 -> DirUp
                           3 -> DirDown
               in (d, g')

oppositeDir :: Direction -> Direction
oppositeDir DirLeft = DirRight
oppositeDir DirRight = DirLeft
oppositeDir DirUp = DirDown
oppositeDir DirDown = DirUp

doFromTo :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
doFromTo from to action =
    let loop n | n > to = return ()
               | otherwise = action n >> loop (n+1)
    in loop from
