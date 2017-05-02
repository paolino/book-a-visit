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
{-# language ViewPatterns #-}

module World where

import Status
import Control.Lens.TH (makeLenses)
import Control.Lens hiding (dropping)
import Valid
import Data.Maybe (fromJust)
import qualified Data.Map as M
import Data.Map (Map)

newtype Idx (u :: Role) (s::Phase) = Idx Integer deriving (Eq, Ord, Show)
type MapW (s :: Phase) (u :: Role) a = Map (Idx u s) (Transaction s u a)

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

instance Monoid (World a) where
  mempty = World mempty mempty mempty mempty mempty mempty mempty
  World s a v f q e h `mappend` World s' a' v' f' q' e' h' =
    World   (s `mappend` s')
            (a `mappend` a')
            (v `mappend` v')
            (f `mappend` f')
            (q `mappend` q')
            (e `mappend` e')
            (h `mappend` h')

data Chatting s a = Chatting (Idx Some s) (Chat a)

data StepT = NewT | OtherT

data Protocol r u a where
  New :: Bargain a -> Slot a -> Zone u a -> Protocol NewT u a
  Abort :: Idx u ProposalT -> Protocol OtherT u a
  Appointment :: Idx (Opponent u) ProposalT  -> Place u a -> Protocol OtherT u a
  ChatWaiting :: Chatting WaitingT a -> Protocol OtherT u a
  ChatServing :: Chatting ServingT a -> Protocol OtherT u a
  ChatReleasing :: Chatting ReleasingT a -> Protocol OtherT u a
  StartDrop :: Idx Some WaitingT -> Protocol OtherT Giver a
  Fail :: Idx Some ServingT  -> Failure a -> Protocol OtherT Giver a
  Success :: Idx Some ReleasingT  -> Feedback a -> Protocol OtherT Taker a
  EndDrop :: Idx Some DroppingT -> Feedback a -> Protocol OtherT Taker a


class Step m r a where
  type Output m r a
  data Input m r a
  step :: Input  m r a -> World a -> Output  m r a

data Event r a = FromGiver (Part Giver a) (Protocol r Giver a) | FromTaker (Part Taker a) (Protocol r Taker a) | Tick (Time a)

data Except = IndexNotFound | WrongAuthor deriving Show


class SlotMatch a where
  data Time a
  matchHigh :: Slot a -> Time a -> Bool
  matchLow :: Slot a -> Time a -> Bool


correct :: (t2 -> a) -> Map (Idx t1 t) t2 -> Map (Idx u s) a
correct t = M.fromList . map f . M.assocs where
  f (Idx i,x) = (Idx i, t x)



move :: Lens' (World a) (MapW s u a)
     -> Lens' (World a) (MapW s' u' a)
     -> Integer
     -> World a
      -> (Transaction s' u' a -> Maybe Except)
     -> (Transaction s' u' a -> Transaction s u a)
     -> Output m OtherT a
move l1 l2 j w c f =     case w ^. l2 . at (Idx j) of
      Nothing -> Left IndexNotFound
      Just x -> case c x of
                  Nothing -> Right $ (l1 . at (Idx j) .~ Just (f x))  . (l2 . at (Idx j) .~ Nothing) $ w
                  Just e ->  Left e

noCheck = const Nothing

instance Monad m => Step m NewT a where
  -- step :: forall a  . (SlotMatch a, Monad ) =>  Integer -> Event a -> World a ->  (Either Except (World a))
  type Output m NewT a =  m (World a)
  data Input m NewT a = NewI (m Integer, Event NewT a)
  step (NewI (new,FromGiver p (New b s z))) w = do
    n <- new
    return $ ((proposalGiver . at (Idx n) .~ Just (Proposal (ProposalData b p z s))) w)

  step (NewI (new,FromTaker p (New b s z))) w = do
    n <- new
    return ((proposalTaker . at (Idx n) .~ Just (Proposal (ProposalData b p z s))) w)

other (OtherI x) = x
instance SlotMatch a => Step m OtherT a where
  type Output m OtherT a = Either Except (World a)
  data Input m OtherT a = OtherI (Event OtherT a)

  step (other -> FromGiver p (Abort i@(Idx j))) w =
    move final proposalGiver j w noCheck $ Aborted . EGiver

  step (other ->FromTaker p (Abort i@(Idx j))) w =
    move final proposalTaker j w noCheck $ Aborted . ETaker

  step (other ->FromGiver p (Appointment i@(Idx j) t)) w =
    move waiting proposalTaker j w noCheck $ \x -> Waiting (ETaker (Acceptance x $ AcceptanceData p t))

  step (other ->FromTaker p (Appointment i@(Idx j) t)) w =
    move waiting proposalGiver j w noCheck $ \x -> Waiting (EGiver (Acceptance x $ AcceptanceData p t))

  step (other ->FromGiver p (ChatWaiting (Chatting i@(Idx j) c))) w =
    move waiting waiting j w noCheck $ \x -> ChattingWaiting x $ EGiver $ RChat c

  step (other ->FromTaker p (ChatWaiting (Chatting i@(Idx j) c))) w =
    move waiting waiting j w noCheck $ \x -> ChattingWaiting x $ ETaker $ RChat c

  step (other ->Tick t) w = let
    (rs,ss) = M.partition (\x -> through (view $ proposal . slot) (summary x) `matchLow` t) $ w ^. waiting
    (ss',ts) = M.partition (\x -> through  (view $ proposal . slot) (summary x) `matchHigh` t) $ w ^. serving
    in Right $ w & waiting .~ rs & serving .~ (ss' `mappend` correct Serving ss) & releasing %~ (mappend $ correct Releasing ts)

  step (other ->FromGiver p (ChatServing (Chatting i@(Idx j) c))) w =
    move serving serving j w noCheck $ \x -> ChattingServing x $ EGiver $ RChat c

  step (other ->FromTaker p (ChatServing (Chatting i@(Idx j) c))) w =
    move serving serving j w noCheck $ \x -> ChattingServing x $ ETaker $ RChat c

  step (other ->FromGiver p (ChatReleasing (Chatting i@(Idx j) c))) w =
    move releasing releasing j w noCheck $ \x -> ChattingReleasing x $ EGiver $ RChat c

  step (other ->FromTaker p (ChatReleasing (Chatting i@(Idx j) c))) w =
    move releasing releasing j w noCheck $ \x -> ChattingReleasing x $ ETaker $ RChat c

  step (other ->FromGiver p (StartDrop (Idx j))) w =
    move dropping waiting j w noCheck Dropping

  step (other ->FromGiver p (Fail (Idx j) r)) w =
    move final serving j w noCheck $ flip Failure r

  step (other ->FromTaker p (Success (Idx j) r)) w =
    move final releasing j w noCheck $ flip Successed r

  step (other ->FromTaker p (EndDrop (Idx j) r)) w =
    move final dropping j w noCheck $ flip Dropped r
