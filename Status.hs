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
{-# language PolyKinds #-}
{-# language TypeInType #-}

module Status where

import Data.Bifunctor
import Control.Lens
import Data.Data.Lens
import Data.Data
import Data.Typeable
import Control.Lens.TH
import GHC.Base

-- | kind to distinguish the presence of a role
data Role = Giver | Taker


data Presence (a :: kr)  where
  Present :: a -> Presence a
  Absent :: Presence a
-- | kind to distinguish the transaction phases
data Phase = BootT | ProposalT | WaitingT | DroppedT | ServingT | ReleasingT | DroppingT | FinalT | AcceptanceT

-- | types of the identifications for both roles (what about Some ?) -- FIXIT
type family Part (u :: Role) a

type family Opponent u where
  Opponent Giver = Taker
  Opponent Taker = Giver

data ERole x y = EGiver x | ETaker y deriving (Show,Eq)

makePrisms ''ERole

instance Bifunctor ERole where
  bimap f g (EGiver x) = EGiver (f x)
  bimap f g (ETaker x) = ETaker (g x)

type Roled f (a :: k) = ERole (f Giver a)  (f Taker a)

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

type family UnPresent b a where
  UnPresent (Present u) a = ConstraintsUA Show u a
  UnPresent Absent a = ConstraintsA Show a

type PresenceRoled f (a :: k) = ERole (f (Present Giver) a)  (f (Present Taker) a)

data Transaction s (u :: Presence Role) a where

  Proposal :: ProposalData u a -> Transaction ProposalT (Present u) a
  -- | dropping a proposal
  Aborted :: PresenceRoled (Transaction ProposalT) a -> Transaction FinalT Absent a

  -- | Accepting a proposal (ephimeral)
  Acceptance :: Transaction ProposalT (Present u) a -> AcceptanceData (Opponent u) a -> Transaction AcceptanceT (Present u) a

  -- | chatting before initial status
  Waiting :: PresenceRoled (Transaction AcceptanceT) a -> Transaction WaitingT Absent a

  -- | chatting before recursive status
  ChattingWaiting :: Transaction WaitingT Absent a -> RoledChat a -> Transaction WaitingT Absent a

  -- | Consensual end
  Dropping :: Transaction WaitingT Absent a -> Transaction DroppingT Absent a


  Serving :: Transaction WaitingT Absent a -> Transaction ServingT Absent a

  -- | chatting during service time
  ChattingServing :: Transaction ServingT Absent a -> RoledChat a -> Transaction ServingT Absent a

  Releasing :: Transaction ServingT Absent a -> Transaction ReleasingT Absent a

  ChattingReleasing :: Transaction ReleasingT Absent a -> RoledChat a -> Transaction ReleasingT Absent a

  Successed :: Transaction ReleasingT Absent a -> Feedback a -> Transaction FinalT Absent a

  Dropped :: Transaction DroppingT Absent a -> Feedback a -> Transaction FinalT Absent a

  Failure :: Transaction ServingT Absent a -> Failure a -> Transaction FinalT Absent a

deriving instance (UnPresent u a) => Show (Transaction s u a)


data Summary u a = Summary {
  _proposal :: ProposalData u a,
  _acceptance :: Maybe (AcceptanceData (Opponent u) a),
  _chat :: [RoledChat a],
  _feedback :: Maybe (Either (Failure a) (Feedback a))
}

deriving instance ConstraintsUA Show u a => Show (Summary u a)

makeLenses ''Summary


class SummaryC s u a where
  type SummaryT s u a
  summary :: Transaction s u a -> SummaryT s u a

instance {-# OVERLAPPABLE #-} SummaryC ProposalT (Present u) a where
  type SummaryT ProposalT (Present u) a = Summary u a
  summary (Proposal x) = Summary x Nothing [] Nothing

instance {-# OVERLAPPABLE #-} SummaryC AcceptanceT (Present u) a where
  type SummaryT AcceptanceT (Present u) a = Summary u a
  summary (Acceptance p x) = set acceptance (Just x) $ summary p

type ModT (f :: Role -> * -> * ) (g :: Role -> * -> * ) a = forall u. f u a -> g u a
type ModTP (f :: Presence Role -> * -> * ) (g :: Role -> * -> * ) a = forall u. f (Present u) a -> g u a

onBoth :: ModT f g a -> Roled f a -> Roled g a
onBoth s = bimap s s
onBothP :: ModTP f g a -> PresenceRoled f a -> Roled g a
onBothP s = bimap s s

instance {-# OVERLAPS #-} SummaryC s Absent a where
  type SummaryT s Absent a = Roled Summary a
  summary (Aborted b) = summary `onBothP` b
  summary (Waiting p) = summary `onBothP` p
  summary (ChattingWaiting p x) = over chat (x:) `onBoth`  (summary p) where
  summary (Dropping p) = summary p
  summary (Dropped p x) = set feedback (Just $ Right x) `onBoth` summary p where
  summary (Serving p) = summary p
  summary (ChattingServing p x) =  over chat (x:) `onBoth` summary p where
  summary (Failure p x) = set feedback (Just $ Left x) `onBoth` summary p
  summary (Releasing p) = summary p
  summary (ChattingReleasing p x) =  over chat (x:) `onBoth` summary p where
  summary (Successed p x) = set feedback (Just $ Right x) `onBoth` summary p
    {-
  -}
