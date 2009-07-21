{-# LANGUAGE ExistentialQuantification, FlexibleContexts #-}

module Actor
    ( Event(..)
    , input
    , tileMap
    , evAddActor, evRemoveSelf, evMessage, evMoveCamera, evDebug
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
import Data.List (sortBy)
import Data.Function (on)

import TileMap
import Util
import Render
import Input
import IdentityList (IL)
import qualified IdentityList as IL

-- When actors are being updated, they can yield a list of events
data Event = AddActor AnyActor
           | RemoveActor ActorId
           | SendMessage MessageRec
           | MoveCamera Point2
           | Debug String -- Temp
    deriving Show

-- Actors can send messages to each other, this is the only way in which they can influence each other
data Message = Impact Int Point2
    deriving Show

data MessageRec = MessageRec ActorId ActorId Message -- Sender Receiver Message
    deriving Show

--input :: Act Input
input :: MonadReader UpdateState m => m Input
input = liftM usInput ask

tileMap :: MonadReader UpdateState m => m TileMap
tileMap = liftM usTileMap ask

event :: MonadWriter [Event] m => Event -> m ()
event = tell . return 

evDebug :: MonadWriter [Event] m => String -> m ()
evDebug = event . Debug

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

{-selfId :: MonadReader a m => m ActorId-}
selfId :: MonadReader UpdateState m => m ActorId
selfId = liftM usSelfId ask

-- Monad in which actors are updated
type Act = RandT DefGen (ReaderT UpdateState (Writer [Event]))

runAct :: Act a -> DefGen -> UpdateState -> (a, DefGen, [Event])
runAct a g s = let b = runRandT a g
                   c = runReaderT b s
                   ((res, g'), evs) = runWriter c
               in (res, g', evs)

{-type Act = ReaderT UpdateState (Writer [Event])-}

{-runAct :: Act a -> DefGen -> UpdateState -> (a, DefGen, [Event])-}
{-runAct a g s = let c = runReaderT a s-}
                   {-(res, evs) = runWriter c-}
               {-in (res, g, evs) -}

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

instance Show AnyActor where
    show (AnyActor a) = show a

type ActorId = Int
type ActorList = IL AnyActor

withSelfId :: ActorId -> Act a -> Act a
withSelfId id = local (\s -> s { usSelfId = id })

mapActors :: ((ActorId, AnyActor) -> Act AnyActor) -> ActorList -> Act ActorList
mapActors g acts = IL.mapM f acts
    where f :: (ActorId, AnyActor) -> Act AnyActor
          f arec@(id, actor) = withSelfId id $ g arec

updateActors = mapActors (\(_, AnyActor actor) -> return . AnyActor =<< update actor)

dispatchMessages :: ActorList -> [MessageRec] -> Act ActorList
dispatchMessages = foldl f . return 
    where f :: Act ActorList -> MessageRec -> Act ActorList 
          f mlist (MessageRec from to msg) = do
              list <- mlist
              case to `IL.lookup` list of
                  Just (AnyActor actor) -> do
                      actor' <- withSelfId to $ message msg actor 
                      return $ IL.update to (AnyActor actor') list
                  Nothing -> mlist -- Actor was deleted before message reached it

collisions :: ActorList -> Act ActorList
collisions acts = IL.mapM testAll acts
    where testAll :: (ActorId, AnyActor) -> Act AnyActor
          testAll (id, actor@(AnyActor aa)) = withSelfId id $ IL.foldl' testOne (return actor) acts
              where -- Test intersection of one actor with all other actors
                    testOne :: Act AnyActor -> (ActorId, AnyActor) -> Act AnyActor
                    testOne mactor arec@(id2, AnyActor actor2) 
                        | id2 == id = mactor
                        | otherwise = do
                            AnyActor actor <- mactor 
                            if rectIntersect (posRect actor) (posRect actor2)
                                then liftM AnyActor $ collision arec actor
                                else
                                    return $ AnyActor actor
                                    -- I don't get it! Change that line to just 'mactor' => 0 FPS without -O2
                                    -- But return $ AnyActor actor and mactor should be just the same?
                                    -- It's probably lazy evaluation again :)

zSort :: [(ActorId, AnyActor)] -> [(ActorId, AnyActor)]
zSort = sortBy cmp
    where cmp = flip (compare `on` ypos)
          ypos (_, AnyActor a) = py . rectPos . posRect $ a 

renderActors :: ActorList -> SpriteMap -> IO ()
renderActors acts sprs = mapM_ f . zSort . IL.toList $ acts
    where f (_, AnyActor actor) = render sprs actor 
                                  -- >> rectangle (GL.Color4 1 0 0 0.5) (posRect actor)
