{-# LANGUAGE ExistentialQuantification, FlexibleContexts #-}

module Actor
    ( Event(..)
    , input
    , tileMap
    , evAddActor, evRemoveSelf, evMessage, evMoveCamera
    , selfId
    , Message(Impact)
    , MessageRec
    , UpdateState(UpdateState, usTileMap, usInput, usSelfId)
    , Act, runAct
    , Actor(neededResources, update, render, message, collision, posRect)
    , ActorId, ActorList, AnyActor(AnyActor)
    , updateActors, renderActors, dispatchMessages
    , collisions
    ) where

import Control.Monad.Writer.Strict
import Control.Monad.State.Strict
import Control.Monad.Reader
import qualified Graphics.UI.SDL as SDL (Event)
import qualified Graphics.Rendering.OpenGL.GL as GL
import Control.Monad.Random
import Control.Applicative
import Data.List (sortBy)
import Data.Function (on)

import TileMap
import Util
import Render
import Input
import IdentityList (IL)
import qualified IdentityList as IL
import Control.Parallel

-- When actors are being updated, they can yield a list of events
data Event = AddActor AnyActor
           | RemoveActor ActorId
           | SendMessage MessageRec
           | MoveCamera Point2
    deriving Show

-- Actors can send messages to each other, this is the only way in which they can influence each other
data Message = Impact Int Point2
    deriving Show

data MessageRec = MessageRec ActorId ActorId Message -- Sender Receiver Message
    deriving Show

event :: MonadWriter [Event] m => Event -> m ()
event = tell . return 

evAddActor :: MonadWriter [Event] m => AnyActor -> m ()
evAddActor = event . AddActor

evRemoveSelf :: (MonadReader UpdateState m, MonadWriter [Event] m) => m ()
evRemoveSelf = ask >>= event . RemoveActor . usSelfId

evMessage :: (MonadReader UpdateState m, MonadWriter [Event] m) => ActorId -> Message -> m ()
evMessage to msg = ask >>= \us -> event $ SendMessage $ MessageRec (usSelfId us) to msg 

evMoveCamera :: MonadWriter [Event] m => Point2 -> m ()
evMoveCamera = event . MoveCamera

-- Reader state for actors when updating
data UpdateState = UpdateState { usTileMap :: TileMap
                               , usInput :: Input
                               , usSelfId :: ActorId 
                               }

selfId :: MonadReader UpdateState m => m ActorId
selfId = liftM usSelfId ask

input :: MonadReader UpdateState m => m Input
input = liftM usInput ask

tileMap :: MonadReader UpdateState m => m TileMap
tileMap = liftM usTileMap ask

-- Monad in which actors are updated
type Act = RandT DefGen (ReaderT UpdateState (Writer [Event]))

runAct :: Act a -> DefGen -> UpdateState -> (a, DefGen, [Event])
runAct a g s = let b = runRandT a g
                   c = runReaderT b s
                   ((res, g'), evs) = runWriter c
               in (res, g', evs)

-- Actors need to be an instance of this type class
class Show a => Actor a where
    neededResources :: a -> [FilePath]
    neededResources _ = []

    update :: a -> Act a
    render :: SpriteMap -> a -> IO ()

    message :: Message -> a -> Act a
    message _ = return

    posRect :: a -> Rect
    posRect = undefined

    isSolid :: a -> Bool
    isSolid _ = True

    collision :: (ActorId, AnyActor) -> a -> Act a
    collision _ = return

data AnyActor = forall a. Actor a => AnyActor a

instance Actor AnyActor where
    neededResources (AnyActor a) = neededResources a
    update (AnyActor a) = AnyActor <$> update a
    render sm (AnyActor a) = render sm a
    message m (AnyActor a) = AnyActor <$> message m a
    posRect (AnyActor a) = posRect a
    isSolid (AnyActor a) = isSolid a
    collision aa (AnyActor a) = AnyActor <$> collision aa a

instance Show AnyActor where
    show (AnyActor a) = show a

type ActorId = Int
type ActorList = IL AnyActor

addActor :: AnyActor -> ActorList -> ActorList
addActor = IL.insert

removeActor :: ActorId -> ActorList -> ActorList
removeActor = IL.delete

withSelfId :: ActorId -> Act a -> Act a
withSelfId id = local (\s -> s { usSelfId = id })

mapActors :: ((ActorId, AnyActor) -> Act AnyActor) -> ActorList -> Act ActorList
mapActors g acts = IL.mapM f acts
    where f :: (ActorId, AnyActor) -> Act AnyActor
          f actor@(id, _) = withSelfId id $ g actor

updateActors = mapActors $ update . snd 

dispatchMessages :: ActorList -> [MessageRec] -> Act ActorList
dispatchMessages = foldl f . return 
    where f :: Act ActorList -> MessageRec -> Act ActorList 
          f mlist (MessageRec from to msg) = do
              list <- mlist
              case to `IL.lookup` list of
                  Just actor -> do
                      actor' <- withSelfId to $ message msg actor 
                      return $ IL.update to actor' list
                  Nothing -> return list -- Actor was deleted before message reached it

collisions :: ActorList -> Act ActorList
collisions acts = mapActors testAll acts
    where testAll :: (ActorId, AnyActor) -> Act AnyActor
          testAll (id1, actor) = foldM testOne actor $ IL.toList acts
              where -- Test intersection of one actor with all other actors
                    testOne :: AnyActor -> (ActorId, AnyActor) -> Act AnyActor
                    testOne actor1 arec@(id2, actor2) =
                        if id1 == id2 || rectIntersect (posRect actor) (posRect actor2)
                            then collision arec actor
                            else return actor

zSort :: [(ActorId, AnyActor)] -> [(ActorId, AnyActor)]
zSort = sortBy cmp
    where cmp = flip (compare `on` ypos)
          ypos (_, a) = py . rectPos . posRect $ a 

renderActors :: ActorList -> SpriteMap -> IO ()
renderActors acts sprs = mapM_ f . zSort . IL.toList $ acts
    where f (_, actor) = render sprs actor 
                         >> rectangle (GL.Color4 1 0 0 0.5) (posRect actor)
