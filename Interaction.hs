{-# language TypeFamilies #-} -- free implementations
{-# language TemplateHaskell #-} -- lens for world an bargain
                                 -- {-# language InstanceSigs #-}
{-# language DataKinds#-} -- phases of interaction at type level
{-# language GADTs #-}  -- dating and interaction

{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}

{-# language ConstraintKinds #-} -- synonyms of constraints

{-# language MultiParamTypeClasses#-} -- include and checker

{-# language StandaloneDeriving #-} -- show and eq

{-# language UndecidableInstances #-} --everywhere


{-# language Rank2Types #-} -- ProposalLens definition
{-# language TypeInType #-}

module Interaction where

import Control.Lens.TH (makeLenses)
import Control.Lens ((^.),(.~), Lens',over, set)
import Data.Bifunctor (first)
import Data.Maybe (fromJust)
import GHC.Base (Constraint, Type)


import Locations
import Valid


-- | role distinction at type level
data Role = Giver | Taker

type instance Opponent Giver = Taker
type instance Opponent Taker = Giver

data family Roles (r :: Role) a

-- | time span
data family Slot a


-- | core semantic for service
type family Bargain a

-- | informal comunication
type family Chat a

-- | final taker expression
type family Feedback a

-- | final alternative giver act
type family Failure a


type Showers a =
  (   Show (Chat a)
  ,   Show (Roles Giver a)
  ,   Show (Roles Taker a)
  ,   Show (Feedback a)
  ,   Show (Failure a)
  ,   Show (Place Taker a)
  ,   Show (Place Giver a)
  ,   Show (Zone Taker a)
  ,   Show (Zone Giver a)
  ,   Show (Slot a)
  ,   Show (Bargain a)
  )


data Offer u a = Offer {
  _bargain :: Bargain a ,
  _time :: Slot a ,
  _proponent :: Roles u a
                       }

makeLenses '' Offer

type Eqs a =  (Eq (Bargain a), Eq (Slot a),
  Eq (Roles 'Taker a), Eq (Roles Giver a),
   Eq (Place 'Giver a), Eq (Place 'Taker a),
   Eq (Zone 'Giver a), Eq (Zone 'Taker a),
   Eq (Bargain a), Eq (Feedback a), Eq (Chat a), Eq (Failure a))
deriving instance Eqs a => Eq (Offer Taker a)
deriving instance Eqs a => Eq (Offer Giver a)

deriving instance (Showers a, Show (Roles u a)) => Show (Offer u a)

data  Open u a = Open (Offer u a) (Proposal u a)

deriving instance (Showers a) => Show (Open Giver a)
deriving instance (Showers a) => Show (Open Taker a)

data Appointment u a  = Appointment (Offer (Opponent u) a) (Roles u a) (Acceptance u a)

deriving instance (Showers a) => Show (Appointment Giver a)
deriving instance (Showers a) => Show (Appointment Taker a)

deriving instance  Eqs a =>  Eq (Appointment Giver a)
deriving instance  Eqs a => Eq (Appointment Taker a)


instance (Eq (Bargain a), Eq (Slot a),Eq (Place u' a), ZonePlace u a, Symmetric u u', Eq (Roles u' a), Eq (Offer u' a)) => Valid (Appointment u a, Open u' a) where
  valid (Appointment a' _ l' , Open a l ) = a == a'  && valid (l,l')

-- | Interaction phase, after boxing the dating phase, parts chat and conclude
data  Interaction a where
  -- | boxing a Dating taker proposed
  FromTaker :: Appointment Taker a ->  Interaction a
  -- | boxing a Dating giver proposed
  FromGiver :: Appointment Giver a ->  Interaction a
  -- | Chatting recursive state
  ChatTaker ::  Chat a ->  Interaction a ->  Interaction a
  -- | Chatting recursive state
  ChatGiver ::  Chat a ->  Interaction a ->  Interaction a

type Boxer u a = Appointment u a -> Interaction a

data End a where
  -- | a succesfully completed interaction (from taker)
  Positive :: Feedback a ->  Interaction a ->   End a
  -- | a failed appointment (from giver)
  Negative :: Failure a ->  Interaction a ->   End a
  -- | premature consensual end (from giver)
  Abandon ::  Interaction a ->  End a

deriving instance  Eqs a => Eq (Interaction a)
deriving instance Showers a => Show (Interaction a)
deriving instance  Eqs a => Eq (End a)
deriving instance Showers a => Show (End a)


