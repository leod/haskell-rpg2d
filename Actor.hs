{-# LANGUAGE ExistentialQuantification #-}

module Actor (
    Event(AddActor, RemoveActor), -- Should be abstract, but needed to pattern-match by GameState
    evAddActor, evRemoveSelf,
    UpdateState(UpdateState, usTileMap, usSelfId),
    Act, runAct,
    Actor(update, render),
    ActorID, ActorRec(ActorRec), AnyActor(AnyActor),
    updateActors, renderActors,
    ) where

import Control.Monad.Random
import Control.Monad.Writer
import Control.Monad.Reader

import TileMap
import Util

-- When actors are being updated, they can yield a list of events
data Event = AddActor AnyActor
           | RemoveActor ActorID
    deriving Show

event :: Event -> Act ()
event = tell . return 

evAddActor :: (Actor a) => a -> Act ()
evAddActor = event . AddActor . AnyActor

evRemoveSelf :: Act ()
evRemoveSelf = ask >>= event . RemoveActor . usSelfId

-- Reader state for actors when updating
data UpdateState = UpdateState {
      usTileMap :: TileMap
    , usSelfId :: ActorID
}

-- Monad in which actors are updated
type Act a = RandT DefGen (ReaderT UpdateState (Writer [Event])) a

runAct :: StdGen -> UpdateState -> Act a -> ([Event], a, StdGen)
runAct g s a = let b = runRandT a g
                   c = runReaderT b s
                   ((actor, g'), evs) = runWriter c
               in (evs, actor, g') 

-- Actors need to be an instance of this type class
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

mapActors :: (ActorRec -> Act ActorRec) -> [ActorRec] -> Act [ActorRec]
mapActors g acts = mapM f acts
    where
        f :: ActorRec -> Act ActorRec
        f a@(ActorRec id _) = local (\s -> s { usSelfId = id }) (g a) >>= (\a -> return a)

renderActors :: [ActorRec] -> IO ()
renderActors acts = mapM_ f acts
    where
        f :: ActorRec -> IO ()
        f (ActorRec id (AnyActor actor)) = render actor
