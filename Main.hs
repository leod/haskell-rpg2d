{-# LANGUAGE ExistentialQuantification, GeneralizedNewtypeDeriving #-}

-- Some mindstorming

import Data.Array (Array, array)
import Control.Monad.Random
import Control.Monad.Writer
import Control.Monad.Reader

type Tile = Int

type Point2 = (Int, Int)
type TileMap = Array Point2 Tile

data Event = AddActor AnyActor
           | RemoveActor ActorID

type DefGen = StdGen

data UpdateState = UpdateState {
      tileMap :: TileMap
    , selfId :: ActorID
}

type Act a = RandT DefGen (ReaderT UpdateState (Writer [Event])) a

runAct :: StdGen -> UpdateState -> Act a -> ([Event], a, StdGen)
runAct g s a = let b = runRandT a g
                   c = runReaderT b s
                   ((actor, g'), evs) = runWriter c
               in (evs, actor, g') 

class Show a => Actor a where
    update :: a -> Act a

data AnyActor = forall a. Actor a => AnyActor a

instance Show AnyActor where
    show (AnyActor a) = show a

type ActorID = Int
data ActorRec = ActorRec ActorID AnyActor
    deriving Show

updateActors :: [ActorRec] -> Act [ActorRec]
updateActors acts = mapM f acts
    where
        f :: ActorRec -> Act ActorRec
        f (ActorRec id (AnyActor actor)) = local (\s -> s { selfId = id }) (update actor) >>= (\a -> return $ ActorRec id (AnyActor a))

data NPC = NPC {
      pos :: Point2
} deriving Show

instance Actor NPC where
    update self = do x' <- getRandomR (100, 200)
                     y' <- getRandomR (100, 200)
                     return $ self { pos = (x', y') }

main = do print actors

          let (_, actors', g) = runAct (mkStdGen 100) (UpdateState tm undefined) (updateActors actors)
          print actors'

          let (_, actors'', g') = runAct g (UpdateState tm undefined) (updateActors actors')
          print actors''

    where actors = [ActorRec 0 (AnyActor $ NPC { pos = (1, 2) }) ]
          tm = array ((0, 0), (100, 100)) [((x, y), y*100+x) | x <- [0..100], y <- [0..100]]
