{-# language TemplateHaskell #-}
{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language StandaloneDeriving #-}
{-# language ConstraintKinds #-}
{-# language GADTs #-}
{-# language UndecidableInstances #-}
{-# language TypeFamilies #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds #-}
{-# language FunctionalDependencies #-}
{-# language ScopedTypeVariables #-}
{-# language Rank2Types#-}

module World where

import Status
import Control.Lens.TH (makeLenses)
import Control.Lens hiding (dropping)
import Valid
import Data.Maybe (fromJust)
import qualified Data.Map as M
import Data.Map (Map)

newtype Idx (s :: Phase) (u :: Role) = Idx Integer deriving (Eq, Ord, Show)
type MapW (s :: Phase) (u :: Role) a = Map (Idx s u) (Transaction s u a)

data World a = World
  {   _proposalGiver  ::  MapW ProposalT Giver a
  ,   _proposalTaker  ::  MapW ProposalT Taker a
  ,   _waiting        ::  MapW WaitingT Some a
  ,   _dropping        ::  MapW DroppingT Some a
  ,   _serving        ::  MapW ServingT Some a
  ,   _releasing      ::  MapW ReleasingT Some a
  ,   _final          ::  MapW FinalT Some a
  }

makeLenses ''World
data Chatting s a = Chatting (Idx s Some) (Chat a)

data Protocol u a where
  New :: Bargain a -> Slot a -> Zone u a -> Protocol u a
  Abort :: Idx ProposalT u -> Protocol u a
  Appointment :: Idx ProposalT (Opponent u) -> Place u a -> Protocol u a
  ChatWaiting :: Chatting WaitingT a -> Protocol u a
  ChatServing :: Chatting ServingT a -> Protocol u a
  ChatReleasing :: Chatting ReleasingT a -> Protocol u a
  StartDrop :: Idx WaitingT Some -> Protocol Giver a
  Fail :: Idx ServingT Some -> Failure a -> Protocol Giver a
  Success :: Idx ReleasingT Some -> Feedback a -> Protocol Taker a
  EndDrop :: Idx DroppingT Some -> Feedback a -> Protocol Taker a

data Event a = FromGiver (Part Giver a) (Protocol Giver a) | FromTaker (Part Taker a) (Protocol Taker a) | Tick (Time a)

data Except = IndexNotFound | WrongAuthor

class SlotMatch a where
  data Time a
  matchHigh :: Slot a -> Time a -> Bool
  matchLow :: Slot a -> Time a -> Bool


correct :: (t2 -> a) -> Map (Idx t t1) t2 -> Map (Idx s u) a
correct t = M.fromList . map f . M.assocs where
  f (Idx i,x) = (Idx i, t x)


move :: Lens' (World a) (MapW s u a)
     -> Lens' (World a) (MapW s' u' a)
     -> Integer
     -> World a
      -> (Transaction s' u' a -> Maybe Except)
     -> (Transaction s' u' a -> Transaction s u a)
     -> Either Except (World a)
move l1 l2 j w c f =     case w ^. l2 . at (Idx j) of
      Nothing -> Left IndexNotFound
      Just x -> case c x of
                  Nothing -> Right $ (l1 . at (Idx j) .~ Just (f x))  . (l2 . at (Idx j) .~ Nothing) $ w
                  Just e -> Left e

noCheck = const Nothing
step :: forall a m . (SlotMatch a, Monad m) => m Integer -> Event a -> World a -> m (Either Except (World a))
step new (FromGiver p (New b s z)) w = let
  f n = proposalGiver . at (Idx n) .~ Just (Proposal (ProposalData b p z s)) $ w
  in Right <$> f <$> new
step new (FromTaker p (New b s z)) w = let
  f n = proposalTaker . at (Idx n) .~ Just (Proposal (ProposalData b p z s)) $ w
  in Right <$> f <$> new

step _ (FromGiver p (Abort i@(Idx j))) w = return $
  move final proposalGiver j w noCheck $ Aborted . EGiver

step _ (FromTaker p (Abort i@(Idx j))) w = return $
  move final proposalTaker j w noCheck $ Aborted . ETaker

step _ (FromGiver p (Appointment i@(Idx j) t)) w = return $
  move waiting proposalTaker j w noCheck $ \x -> Waiting (ETaker (Acceptance x $ AcceptanceData p t))

step _ (FromTaker p (Appointment i@(Idx j) t)) w = return $
  move waiting proposalGiver j w noCheck $ \x -> Waiting (EGiver (Acceptance x $ AcceptanceData p t))

step _ (FromGiver p (ChatWaiting (Chatting i@(Idx j) c))) w = return $
  move waiting waiting j w noCheck $ \x -> ChattingWaiting x $ EGiver $ RChat c

step _ (FromTaker p (ChatWaiting (Chatting i@(Idx j) c))) w = return $
  move waiting waiting j w noCheck $ \x -> ChattingWaiting x $ ETaker $ RChat c

step _ (Tick t) w = return $ let
  (rs,ss) = M.partition (\x -> through (view $ proposal . slot) (summary x) `matchLow` t) $ w ^. waiting
  (ss',ts) = M.partition (\x -> through  (view $ proposal . slot) (summary x) `matchHigh` t) $ w ^. serving
  in Right $ w & waiting .~ rs & serving .~ (ss' `mappend` correct Serving ss) & releasing %~ (mappend $ correct Releasing ts)

step _ (FromGiver p (ChatServing (Chatting i@(Idx j) c))) w = return $
  move serving serving j w noCheck $ \x -> ChattingServing x $ EGiver $ RChat c

step _ (FromTaker p (ChatServing (Chatting i@(Idx j) c))) w = return $
  move serving serving j w noCheck $ \x -> ChattingServing x $ ETaker $ RChat c

step _ (FromGiver p (ChatReleasing (Chatting i@(Idx j) c))) w = return $
  move releasing releasing j w noCheck $ \x -> ChattingReleasing x $ EGiver $ RChat c

step _ (FromTaker p (ChatReleasing (Chatting i@(Idx j) c))) w = return $
  move releasing releasing j w noCheck $ \x -> ChattingReleasing x $ ETaker $ RChat c

step _ (FromGiver p (StartDrop (Idx j))) w = return $
  move dropping waiting j w noCheck Dropping

step _ (FromGiver p (Fail (Idx j) r)) w = return $
  move final serving j w noCheck $ flip Failure r

step _ (FromTaker p (Success (Idx j) r)) w = return $
  move final releasing j w noCheck $ flip Successed r

step _ (FromTaker p (EndDrop (Idx j) r)) w = return $
  move final dropping j w noCheck $ flip Dropped r
