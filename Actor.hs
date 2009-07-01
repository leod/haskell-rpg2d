{-# LANGUAGE ExistentialQuantification #-}

module Actor (
    Event(AddActor, RemoveActor, SendMessage),
    evAddActor, evRemoveSelf, evMessage,
    Message(Impact),
    MessageRec,
    UpdateState(UpdateState, usTileMap, usSelfId),
    Act, runAct,
    Actor(update, render, message),
    ActorID, ActorList, AnyActor(AnyActor),
    updateActors, renderActors, dispatchMessages
    ) where

import Control.Monad.Random
import Control.Monad.Writer
import Control.Monad.Reader
import Graphics.UI.SDL (Surface)

import TileMap
import Util
import Resource
import IdentityList (IL)
import qualified IdentityList as IL

-- When actors are being updated, they can yield a list of events
data Event = AddActor AnyActor
           | RemoveActor ActorID
           | SendMessage MessageRec
    deriving Show

-- Actors can send messages to each other, this is the only way in which they can influence each other
data Message = Impact Int Point2
    deriving Show

data MessageRec = MessageRec ActorID ActorID Message -- Sender Receiver Message
    deriving Show

event :: Event -> Act ()
event = tell . return 

evAddActor :: (Actor a) => a -> Act ()
evAddActor = event . AddActor . AnyActor

evRemoveSelf :: Act ()
evRemoveSelf = ask >>= event . RemoveActor . usSelfId

evMessage :: ActorID -> Message -> Act ()
evMessage to msg = ask >>= \us -> event $ SendMessage $ MessageRec (usSelfId us) to msg 

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

    message :: a -> Message -> Act a
    message a _ = return a

data AnyActor = forall a. Actor a => AnyActor a

instance Show AnyActor where
    show (AnyActor a) = show a

type ActorID = Int
type ActorList = IL AnyActor

withSelfId :: ActorID -> Act a -> Act a
withSelfId id = local (\s -> s { usSelfId = id })

mapActors :: (AnyActor -> Act AnyActor) -> ActorList -> Act ActorList
mapActors g acts = IL.mapM f acts
    where
        f :: (ActorID, AnyActor) -> Act AnyActor
        f (id, actor) = withSelfId id (g actor)

updateActors :: ActorList -> Act ActorList
updateActors = mapActors (\(AnyActor actor) ->
    do actor' <- update actor
       return $ AnyActor actor')

dispatchMessages :: [MessageRec] -> ActorList -> Act ActorList
dispatchMessages msgs actors = foldl f (return actors) msgs 
    where
        f :: Act ActorList -> MessageRec -> Act ActorList 
        f mlist (MessageRec from to msg) = do list <- mlist
                                              case to `IL.lookup` list of
                                                  (Just (AnyActor actor)) -> do
                                                      actor' <- withSelfId to (message actor msg)
                                                      return $ IL.update to (AnyActor actor') list


renderActors :: ActorList -> Surface -> Renderer ()
renderActors acts sur = IL.mapM_ f acts
    where
        f (_, AnyActor actor) = render actor sur
