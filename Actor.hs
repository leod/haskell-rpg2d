{-# LANGUAGE ExistentialQuantification #-}

module Actor
    ( Event(..)
    , input
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

import TileMap
import MonadRandom
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

input :: Act Input
input = liftM usInput ask

event :: Event -> Act ()
event = tell . return 

evDebug :: String -> Act ()
evDebug = event . Debug

evAddActor :: AnyActor -> Act ()
evAddActor = event . AddActor

evRemoveSelf :: Act ()
evRemoveSelf = ask >>= event . RemoveActor . usSelfId

evMessage :: ActorId -> Message -> Act ()
evMessage to msg = ask >>= \us -> event $ SendMessage $ MessageRec (usSelfId us) to msg 

evMoveCamera :: Point2 -> Act ()
evMoveCamera = event . MoveCamera

-- Reader state for actors when updating
data UpdateState = UpdateState { usTileMap :: TileMap
                               , usInput :: Input
                               , usSelfId :: ActorId 
                               }

selfId :: Act ActorId
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
                    actor' <- withSelfId to $ message msg actor 
                    return $ IL.update to (AnyActor actor') list
                Nothing -> mlist -- Actor was deleted before message reached it

collisions :: ActorList -> Act ActorList
collisions acts = IL.mapM testAll acts
    where
        testAll :: (ActorId, AnyActor) -> Act AnyActor
        testAll (id, actor@(AnyActor aa)) = withSelfId id $ IL.foldl' testOne (return actor) acts
            where
                -- Test intersection of one actor with all other actors
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
                                -- It's probably lazy evalutation again :)

renderActors :: ActorList -> SpriteMap -> IO ()
renderActors acts sprs = IL.mapM_ f acts
    where
        f (_, AnyActor actor) = render sprs actor 
                                >> rectangle (GL.Color4 1 0 0 0.5) (posRect actor)
