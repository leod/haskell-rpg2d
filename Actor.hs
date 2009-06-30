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
import Graphics.UI.SDL (Surface)

import TileMap
import Util
import Resource

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

runAct :: Act a -> StdGen -> UpdateState -> ([Event], a, StdGen)
runAct a g s = let b = runRandT a g
                   c = runReaderT b s
                   ((actor, g'), evs) = runWriter c
               in (evs, actor, g') 

-- Actors need to be an instance of this type class
class Show a => Actor a where
    update :: a -> Act a
    render :: a -> Surface -> Renderer ()

data AnyActor = forall a. Actor a => AnyActor a

instance Show AnyActor where
    show (AnyActor a) = show a

type ActorID = Int
data ActorRec = ActorRec ActorID AnyActor
    deriving Show

mapActors :: (AnyActor -> Act AnyActor) -> [ActorRec] -> Act [ActorRec]
mapActors g acts = mapM f acts
    where
        f :: ActorRec -> Act ActorRec
        f (ActorRec id actor) =
            do actor' <- local (\s -> s { usSelfId = id }) (g actor)
               return (ActorRec id actor')

updateActors :: [ActorRec] -> Act [ActorRec]
updateActors = mapActors (\(AnyActor actor) ->
    do actor' <- update actor
       return $ AnyActor actor')

renderActors :: [ActorRec] -> Surface -> Renderer ()
renderActors acts sur = mapM_ f acts
    where
        f (ActorRec _ (AnyActor actor)) = render actor sur
