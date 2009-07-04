{-# LANGUAGE ExistentialQuantification #-}

module Actor
    ( Event(AddActor, RemoveActor, SendMessage)
    , evAddActor, evRemoveSelf, evMessage
    , Message(Impact)
    , MessageRec
    , UpdateState(UpdateState, usTileMap, usSelfId)
    , Act, runAct
    , Actor(neededResources, update, render, message, collision, posRect)
    , ActorId, ActorList, AnyActor(AnyActor)
    , updateActors, renderActors, dispatchMessages
    , collisions
    ) where

import Control.Monad.Random
import Control.Monad.Writer
import Control.Monad.Reader
import Graphics.UI.SDL (Surface)

import TileMap
import Util
import Render
import IdentityList (IL)
import qualified IdentityList as IL

-- When actors are being updated, they can yield a list of events
data Event = AddActor AnyActor
           | RemoveActor ActorId
           | SendMessage MessageRec
    deriving Show

-- Actors can send messages to each other, this is the only way in which they can influence each other
data Message = Impact Int Point2
    deriving Show

data MessageRec = MessageRec ActorId ActorId Message -- Sender Receiver Message
    deriving Show

event :: Event -> Act ()
event = tell . return 

evAddActor :: AnyActor -> Act ()
evAddActor = event . AddActor

evRemoveSelf :: Act ()
evRemoveSelf = ask >>= event . RemoveActor . usSelfId

evMessage :: ActorId -> Message -> Act ()
evMessage to msg = ask >>= \us -> event $ SendMessage $ MessageRec (usSelfId us) to msg 

-- Reader state for actors when updating
data UpdateState = UpdateState { usTileMap :: TileMap
                               , usSelfId :: ActorId -- The id of the actor currently being updated. Not sure if this should be passed as a parameter to update instead...
                               }

-- Monad in which actors are updated
type Act a = RandT DefGen (ReaderT UpdateState (Writer [Event])) a

runAct :: Act a -> StdGen -> UpdateState -> (a, StdGen, [Event])
runAct a g s = let b = runRandT a g
                   c = runReaderT b s
                   ((actor, g'), evs) = runWriter c
               in (actor, g', evs) 

-- Actors need to be an instance of this type class
class Show a => Actor a where
    neededResources :: a -> [FilePath]

    update :: a -> Act a
    render :: a -> SpriteMap -> IO ()

    message :: a -> Message -> Act a
    message a _ = return a

    posRect :: a -> Rect
    posRect = undefined

    isSolid :: a -> Bool
    isSolid _ = True

    collision :: a -> (ActorId, AnyActor) -> Act a
    collision a _ = return a

data AnyActor = forall a. Actor a => AnyActor a

instance Show AnyActor where
    show (AnyActor a) = show a

type ActorId = Int
type ActorList = IL AnyActor

withSelfId :: ActorId -> Act a -> Act a
withSelfId id = local (\s -> s { usSelfId = id })

mapActors :: (AnyActor -> Act AnyActor) -> ActorList -> Act ActorList
mapActors g acts = IL.mapM f acts
    where
        f :: (ActorId, AnyActor) -> Act AnyActor
        f (id, actor) = withSelfId id (g actor)

updateActors :: ActorList -> Act ActorList
updateActors = mapActors (\(AnyActor actor) -> do
    actor' <- update actor
    return $ AnyActor actor')

dispatchMessages :: ActorList -> [MessageRec] -> Act ActorList
dispatchMessages = foldl f . return 
    where
        f :: Act ActorList -> MessageRec -> Act ActorList 
        f mlist (MessageRec from to msg) = do
            list <- mlist
            case to `IL.lookup` list of
                Just (AnyActor actor) -> do
                    actor' <- withSelfId to $ message actor msg
                    return $ IL.update to (AnyActor actor') list
                Nothing -> mlist -- Actor was deleted before message reached it

collisions :: ActorList -> Act ActorList
collisions acts = IL.mapM f acts
    where
        f :: (ActorId, AnyActor) -> Act AnyActor
        f (id, actor@(AnyActor aa)) = withSelfId id $ IL.foldl g (return actor) acts
            where
                -- Test intersection of one actor with all other actors
                g :: Act AnyActor -> (ActorId, AnyActor) -> Act AnyActor
                g mactor rec@(id2, (AnyActor actor2)) 
                    | id2 == id = mactor
                    | otherwise = do
                        AnyActor actor <- mactor 

                        if rectIntersect (posRect actor) (posRect actor2) 
                            then collision actor rec >>= return . AnyActor
                            else mactor

renderActors :: ActorList -> SpriteMap -> IO ()
renderActors acts sprs = IL.mapM_ f acts
    where
        f (_, AnyActor actor) = render actor sprs
