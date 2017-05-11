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
import Data.Aeson.TH
import Data.Char

-- | kind to distinguish the presence of a role
data Role = Giver | Taker


data Presence (a :: kr)  where
  Present :: a -> Presence a
  Absent :: Presence a
-- | kind to distinguish the transaction phases
data Phase = BootT | ProposalT | WaitingT | DroppedT | ServingT | ReleasingT |  FinalT | AcceptanceT

-- | types of the identifications for both roles (what about Some ?) -- FIXIT
data family Part (u :: Role) a

type family Opponent u where
  Opponent Giver = Taker
  Opponent Taker = Giver

type Reflexive u = (Opponent (Opponent u) ~ u)
data ERole x y = EGiver x | ETaker y deriving (Show,Eq)

same (EGiver x) = x
same (ETaker y) = y
makePrisms ''ERole

instance Bifunctor ERole where
  bimap f g (EGiver x) = EGiver (f x)
  bimap f g (ETaker x) = ETaker (g x)

type Roled f (a :: k) = ERole (f Giver a)  (f Taker a)



data family Zone (u :: Role) a
data family Place (u :: Role) a
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
  UnPresent ('Present u) a = ConstraintsUA Show u a
  UnPresent Absent a = ConstraintsA Show a

type PresenceRoled f (a :: k) = ERole (f (Present Giver) a)  (f (Present Taker) a)
-- | Transaction
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
  Dropped :: Transaction WaitingT Absent a -> Transaction FinalT Absent a

  -- | At the appointment
  Serving :: Transaction WaitingT Absent a -> Transaction ServingT Absent a

  -- | chatting during service time
  ChattingServing :: Transaction ServingT Absent a -> RoledChat a -> Transaction ServingT Absent a

  -- | Visit over
  Releasing :: Transaction ServingT Absent a -> Transaction ReleasingT Absent a
  -- | Thanks
  ChattingReleasing :: Transaction ReleasingT Absent a -> RoledChat a -> Transaction ReleasingT Absent a
  
  -- | Yes, we have it, the feedback
  Successed :: Transaction ReleasingT Absent a -> Feedback a -> Transaction FinalT Absent a

  -- | Oh no, a no-show
  Failure :: Transaction ServingT Absent a -> Transaction FinalT Absent a

  -- | Noone took the proposal  
  ExpiredProposal :: PresenceRoled (Transaction ProposalT) a -> Transaction FinalT Absent a

  -- | No feedback in a decent time
  ExpiredReleasing :: Transaction ReleasingT Absent a  -> Transaction FinalT Absent a



deriving instance (UnPresent u a) => Show (Transaction s u a)

data Overdue a = Ok (Feedback a) | NoShow | Expired | Renounce 

deriving instance Show (Feedback a) => Show (Overdue a)

data Summary u a = Summary {
  _proposal :: ProposalData u a,
  _acceptance :: Maybe (AcceptanceData (Opponent u) a),
  _chat :: [RoledChat a],
  _feedback :: Maybe (Overdue a)
}

deriving instance ConstraintsUA Show u a => Show (Summary u a)

makeLenses ''Summary

class SummaryC u a where
  summary :: forall s. Transaction s u a -> Roled Summary a

throughSummary :: SummaryC u a => (forall u. Summary u a -> d) -> Transaction s u a -> ERole d d
throughSummary f y = bimap f f (summary y)

instance SummaryC (Present Giver) a where
  summary (Proposal x) = EGiver $ Summary x Nothing [] Nothing
  summary (Acceptance p x) = EGiver $ set acceptance (Just x) $ (\(EGiver x) -> x) $ summary p

instance SummaryC (Present Taker) a where
  summary (Proposal x)  = ETaker $ Summary x Nothing [] Nothing
  summary (Acceptance p x) = ETaker $ set acceptance (Just x) $ (\(ETaker x) -> x) $ summary p

type ModT (f ::  Role -> * -> * ) (g :: Role -> * -> * ) a = forall u. f u a -> g u a

onBoth :: ModT f g a -> Roled f a -> Roled g a
onBoth s = bimap s s
    {-
type ModTP (f :: Presence Role -> * -> * ) (g :: Role -> * -> * ) a = forall u. f (Present u) a -> g u a
onBothP :: ModTP f g a -> PresenceRoled f a -> Roled g a
onBothP s = bimap s s
-}
throughP :: (forall u . SummaryC u a => Transaction s u a -> b)
             -> PresenceRoled (Transaction s) a -> b
throughP f (EGiver x) = f x
throughP f (ETaker x) = f x

through :: (forall u . f u a -> b) -> Roled f a -> b
through f (EGiver x) = f x
through f (ETaker x) = f x

instance  SummaryC Absent a where
  summary (Aborted b) = summary `throughP` b
  summary (Waiting p) = summary `throughP` p
  summary (ChattingWaiting p x) = over chat (x:) `onBoth`  (summary p) where
  summary (Dropped p) = set feedback (Just Renounce) `onBoth` summary p
  summary (Serving p) = summary p
  summary (ChattingServing p x) =  over chat (x:) `onBoth` summary p where
  summary (Failure p) = set feedback (Just NoShow) `onBoth` summary p
  summary (Releasing p) = summary p
  summary (ChattingReleasing p x) =  over chat (x:) `onBoth` summary p where
  summary (Successed p x) = set feedback (Just $ Ok x) `onBoth` summary p
  summary (ExpiredProposal x) = set feedback (Just Expired) `onBoth` (summary `throughP` x)
  summary (ExpiredReleasing x) = set feedback (Just Expired) `onBoth` summary x

    {-

  -}
