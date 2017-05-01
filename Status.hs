{-# language DataKinds #-}
{-# language TypeFamilies #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
{-# language DeriveDataTypeable #-}
{-# language StandaloneDeriving #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language UndecidableInstances #-}
{-# language Rank2Types #-}
{-# language TemplateHaskell #-}
{-# language ScopedTypeVariables #-}
{-# language ConstraintKinds #-}

module Status where

import Data.Bifunctor
import Control.Lens
import Data.Data.Lens
import Data.Data
import Data.Typeable
import Control.Lens.TH
import GHC.Base


data Role = Giver | Taker | Some
data Phase = BootT | ProposalT | WaitingT | DroppedT | ServingT | ReleasingT | DroppingT | FinalT | AcceptanceT

type family Part (u :: Role) a

type family Opponent u where
  Opponent Giver = Taker
  Opponent Taker = Giver

data ERole x y = EGiver x | ETaker y deriving (Show,Eq)

makePrisms ''ERole

instance Bifunctor ERole where
  bimap f g (EGiver x) = EGiver (f x)
  bimap f g (ETaker x) = ETaker (g x)

type Roled f a = ERole (f Giver a)  (f Taker a)

through :: (forall u . f u a -> b) -> Roled f a -> b
through f (EGiver x) = f x
through f (ETaker x) = f x

type family Zone (u :: Role) a
type family Place (u :: Role) a
type family Slot a
type family Bargain a
type family Chat a
type family Feedback a
type family Failure a

type Classes (c :: * -> Constraint)  a = (c (Failure a), c (Feedback a), c (Chat a), c (Bargain a), c (Slot a))
type ClassesU (c :: * -> Constraint)  u a = ( c (Part u a), c (Zone u a), c (Place u a))
type ConstraintsA (c :: * -> Constraint) a = (ClassesU Show Giver a, ClassesU Show Taker a, Classes Show a)
type ConstraintsUA (c :: * -> Constraint) u a = (ClassesU c u a,ClassesU c (Opponent u) a,ClassesU c Giver a, ClassesU c Taker a ,Classes c a)

newtype RChat (u :: Role) a = RChat (Chat a)
type RoledChat a = Roled RChat a

deriving instance ConstraintsA Show a => Show (RChat u a)

data ProposalData u a = ProposalData {
  _bargain :: Bargain a,
  _proponent :: Part u a,
  _zone :: Zone u a,
  _slot :: Slot a
  }

makeLenses ''ProposalData
deriving instance (Classes Show a, ClassesU Show u a) => Show (ProposalData u a)
deriving instance (Classes Show a, ClassesU Show u a) => Show (AcceptanceData u a)
data AcceptanceData u a = AcceptanceData {
  _accepter :: Part u a,
  _place :: Place u a
  }

makeLenses ''AcceptanceData

data Transaction s (u :: Role) a where

  Proposal :: ProposalData u a -> Transaction ProposalT u a

  -- | dropping a proposal
  Aborted :: Roled (Transaction ProposalT) a -> Transaction FinalT Some a

  -- | Accepting a proposal (ephimeral)
  Acceptance :: Transaction ProposalT u a -> AcceptanceData (Opponent u) a -> Transaction AcceptanceT u a

  -- | chatting before initial status
  Waiting :: Roled (Transaction AcceptanceT) a -> Transaction WaitingT Some a

  -- | chatting before recursive status
  ChattingWaiting :: Transaction WaitingT Some a -> RoledChat a -> Transaction WaitingT Some a

  -- | Consensual end
  Dropping :: Transaction WaitingT Some a -> Transaction DroppingT Some a


  Serving :: Transaction WaitingT Some a -> Transaction ServingT Some a

  -- | chatting during service time
  ChattingServing :: Transaction ServingT Some a -> RoledChat a -> Transaction ServingT Some a

  Releasing :: Transaction ServingT Some a -> Transaction ReleasingT Some a

  ChattingReleasing :: Transaction ReleasingT Some a -> RoledChat a -> Transaction ReleasingT Some a

  Successed :: Transaction ReleasingT Some a -> Feedback a -> Transaction FinalT Some a

  Dropped :: Transaction DroppingT Some a -> Feedback a -> Transaction FinalT Some a

  Failure :: Transaction ServingT Some a -> Failure a -> Transaction FinalT Some a

deriving instance ConstraintsUA Show u a => Show (Transaction s u a)



data Summary u a = Summary {
  _proposal :: ProposalData u a,
  _acceptance :: Maybe (AcceptanceData (Opponent u) a),
  _chat :: [RoledChat a],
  _feedback :: Maybe (Either (Failure a) (Feedback a))
}

deriving instance ConstraintsUA Show u a => Show (Summary u a)

makeLenses ''Summary

type family SummaryT s u a where
  SummaryT AcceptanceT u a = Summary u a
  SummaryT ProposalT u a = Summary u a
  SummaryT s u a = Roled Summary a

class SummaryC s u a where
  summary :: Transaction s u a -> SummaryT s u a

instance {-# OVERLAPPABLE #-} SummaryC ProposalT u a where
    summary (Proposal x) = Summary x Nothing [] Nothing

instance {-# OVERLAPPABLE #-} SummaryC AcceptanceT u a where
  summary (Acceptance p x) = set acceptance (Just x) $ summary p

type ModT (f :: Role -> * -> * ) (g :: Role -> * -> * ) a = forall u. f u a -> g u a

onBoth :: ModT f g a -> Roled f a -> Roled g a
onBoth s = bimap s s

instance {-# OVERLAPS #-} SummaryC s u a where
  summary (Aborted b) = summary `onBoth` b
  summary (Waiting p) = summary `onBoth` p
  summary (ChattingWaiting p x) = over chat (x:) `onBoth`  (summary p) where
  summary (Dropping p) = summary p
  summary (Dropped p x) = set feedback (Just $ Right x) `onBoth` summary p where
  summary (Serving p) = summary p
  summary (ChattingServing p x) =  over chat (x:) `onBoth` summary p where
  summary (Failure p x) = set feedback (Just $ Left x) `onBoth` summary p
  summary (Releasing p) = summary p
  summary (ChattingReleasing p x) =  over chat (x:) `onBoth` summary p where
  summary (Successed p x) = set feedback (Just $ Right x) `onBoth` summary p
  summary _ = error "too much inside to compute a Roled Summary a"

