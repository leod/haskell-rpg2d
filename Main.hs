{-# LANGUAGE ExistentialQuantification #-}

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
    deriving Show

type DefGen = StdGen

data UpdateState = UpdateState {
      usTileMap :: TileMap
    , usSelfId :: ActorID
}

type Act a = RandT DefGen (ReaderT UpdateState (Writer [Event])) a

runAct :: StdGen -> UpdateState -> Act a -> ([Event], a, StdGen)
runAct g s a = let b = runRandT a g
                   c = runReaderT b s
                   ((actor, g'), evs) = runWriter c
               in (evs, actor, g') 

class Show a => Actor a where
    update :: a -> Act a
    render :: a -> IO ()

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
        f (ActorRec id (AnyActor actor)) = local (\s -> s { usSelfId = id }) (update actor) >>= (\a -> return $ ActorRec id (AnyActor a))

renderActors :: [ActorRec] -> IO ()
renderActors acts = mapM_ f acts
    where
        f :: ActorRec -> IO ()
        f (ActorRec id (AnyActor actor)) = render actor

data NPC = NPC {
      pos :: Point2
} deriving Show

event :: Event -> Act ()
event = tell . return 

instance Actor NPC where
    update self = do x' <- getRandomR (99, 200)
                     y' <- getRandomR (100, 200)

                     when (x' < 150) $
                        event (AddActor (AnyActor (NPC { pos = (x' + 10, y' - 10) })))

                     return $ self { pos = (x', y') }

    render self = undefined

data GameState = GameState {
      gsTileMap :: TileMap
    , gsActors :: [ActorRec]
    , gsActorCounter :: Int
} deriving Show

processEvents :: GameState -> [Event] -> GameState
processEvents gs = foldl f gs
    where
        f :: GameState -> Event -> GameState
        f gs (AddActor a) = let counter = gsActorCounter gs
                                rec = ActorRec counter a
                            in gs { gsActors = rec : (gsActors gs)
                                  , gsActorCounter = counter + 1}
        f gs (RemoveActor id) = let actors = gsActors gs
                                    actors' = filter (\(ActorRec iid _) -> iid /= id) actors
                                in gs { gsActors = actors' }

main = do print actors

          let (evs, actors', g) = runAct (mkStdGen 100) (UpdateState tm undefined) (updateActors actors)
          {-print actors'-}

          let gs = GameState { gsTileMap = tm, gsActors = actors', gsActorCounter = 0 }
          let gs' = processEvents gs evs
          print $ gsActors gs'

          renderActors $ gsActors gs'

          {-let (evs, actors'', g') = runAct g (UpdateState tm undefined) (updateActors actors')-}
          {-print actors''-}
          {-print evs-}

    where actors = [ActorRec 0 (AnyActor $ NPC { pos = (1, 2) }), ActorRec 1 (AnyActor $ NPC { pos = (1, 2) })]
          tm = array ((0, 0), (100, 100)) [((x, y), y*100+x) | x <- [0..100], y <- [0..100]]
