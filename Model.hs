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

module Model where

import Control.Lens.TH (makeLenses)
import Control.Lens ((^.),(.~), Lens',over, set)
import Data.Bifunctor (first)
import Data.Maybe (fromJust)
import GHC.Base (Constraint, Type)


import Locations

------------------- lib ---------------

class Valid a where
  valid :: a -> Bool

tbd = error "to be implemented"
------------------------------------

-- | role distinction at type level
data Role = Giver | Taker

type instance Opponent Giver = Taker
type instance Opponent Taker = Giver

data family Roles (r :: Role) a

-- | time span
data family Slot a

data family Match a

-- | core semantic for service
type family Bargain a



type MatchInterface b = (Include (Match b), Target (Match b) ~ b,Monoid (Match b), Eq b)
type CleanInterface b = (Include b, Target b ~ b,Monoid b ,Eq b)

-- | constraints for implementations
type Interface a =
  (
  CleanInterface (Slot a),
  CleanInterface (Zone Giver a),
  CleanInterface (Zone Taker a),
  MatchInterface (Bargain a),
  MatchInterface (Roles Taker a),
  MatchInterface (Roles Giver a)
  )

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
  ,   Show (Match (Roles Taker a))
  ,   Show (Match (Roles Giver a))
  ,   Show (Match (Bargain a))
  ,   Show (Bargain a)
  )


data Offer u a = Offer {
  _bargain :: Bargain a ,
  _time :: Slot a ,
  _proponent :: Roles u a
                       }

deriving instance (Interface a, Eq (Roles u a)) => Eq (Offer u a)

deriving instance (Showers a, Show (Roles u a)) => Show (Offer u a)

data  Open u a = Open (Offer u a) (Proposal u a)

deriving instance (Showers a) => Show (Open Giver a)
deriving instance (Showers a) => Show (Open Taker a)

data Appointment u a  = Appointment (Offer (Opponent u) a) (Roles u a) (Acceptance u a)

deriving instance (Showers a) => Show (Appointment Giver a)
deriving instance (Showers a) => Show (Appointment Taker a)


instance (Eq (Place u' a), ZonePlace u a, Interface a, Symmetric u u', Eq (Roles u' a)) => Valid (Appointment u a, Open u' a) where
  valid (Appointment a' _ l' , Open a l ) = a == a'  && acceptance l l'

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

deriving instance Showers a => Show (Interaction a)



data World f a = World
  {   _giverOffers :: f (Open Giver a)
  ,   _takerOffers :: f (Open Taker a)
  ,   _appointments :: f (Interaction a)
  ,   _visits :: f (End a)
  }

-- | World field selector for offers, can be giverOffers or takerOffers
type ProposalLens f u a =  Lens' (World f a) (f (Open u a))

-- | Parametric Constraint kind for data accessible World
type WorldConstraint (m :: Type -> Constraint) f a =
  (   m (f (Open Giver a))
  ,   m (f (Open Taker a))
  ,   m (f (Interaction a))
  ,   m (f (End a))
  )

-----------------------------
-- World instances ----------
-----------------------------
--
deriving instance (Showers a, WorldConstraint Show f a) => Show (World f a)

instance (Interface a , WorldConstraint Monoid f a) => Monoid (World f a) where
  mempty = World mempty mempty mempty mempty
  World s a v f `mappend` World s' a' v' f' =
    World   (s `mappend` s')
            (a `mappend` a')
            (v `mappend` v')
            (f `mappend` f')

----------------------------

makeLenses ''World

class WorldAccess f where
  data Ix f -- ^
  get :: Ix f -> f b -> Maybe b
  put :: Ix f -> b -> f b -> f b
  insert :: b -> f b -> (f b, Ix f)
  delete :: Ix f  -> f b -> Maybe (f b)




-- | failing reasons for all transitions
data Problem = IndexNotFound |  InvalidLocationSelection  deriving Show

-- | A possible World modification with an outcome 'r'
type DeltaWorld f r a = World f a -> Either Problem (World f a, r)

----------------------------
-- functional interface ----
-- -----------------------

-- | insert a new offer
newOffer :: (Interface a, WorldAccess f)
         => ProposalLens f u a  -- giver/taker selector
         -> Offer u a -- new offer
         -> Proposal u a -- offer location
         -> DeltaWorld f (Ix f) a -- world modification


newOffer k o p w = let
  x = Open o p
  (t,i) = insert x (w ^. k)
  in return $ (set k t w,i)

onIndex :: WorldAccess f => Ix f -> f a -> (a -> Either Problem b) -> Either Problem b
onIndex i xs f = maybe (Left IndexNotFound) f $ get i xs

-- | dropAny index, decide your auth somewhere else
dropAny :: (Interface a, WorldAccess f)
          => Lens' (World f a) (f b)  -- ^ giver/taker offer selector
          -> Ix f  -- ^ index of the offer to be deleted
          -> DeltaWorld f () a -- ^ world modification
dropAny l i w = maybe (Left IndexNotFound) Right $ do
  os <- delete i (w ^. l)
  return $ (l .~ os $ w, ())


-- | book an supply moving it to an appointment
bookOffer :: (Interface a, WorldAccess f, Reflexive u , Eq (Roles (Opponent u) a), ZonePlace u a, Eq (Place (Opponent u) a))
          => ProposalLens f (Opponent u) a -- ^ giver/taker offer selector
          -> Boxer u a  -- ^ boxing to make a Dating an Interaction
          -> Ix f -- ^ index of the offer to be promoted
          -> Roles u a -- ^ accepter
          -> Acceptance u a -- ^ fixing the offer location
          -> DeltaWorld f () a -- ^ world modification
bookOffer k g i u l w = onIndex i (w ^. k)  $ \x@(Open b l') -> let
                            a = Appointment b u l
                            in case valid (a,x) of
                                True -> Right . flip (,) () . over k (fromJust . delete i) . over appointments (put i $ g a) $ w
                                False -> Left InvalidLocationSelection
-- | chat over an appointment
chatAppointment ::  (WorldAccess f, Interface a) => Ix f -> (Chat a -> Interaction a -> Interaction a) -> Chat a -> DeltaWorld f () a
chatAppointment i t x w = onIndex i (w ^. appointments) $ \y ->
    Right $ (over appointments (put i $ t x y) $ w, ())

-- | close an appointment with a feedback
closeAppointment :: (WorldAccess f, Interface a) => Ix f -> (Interaction a -> End a) -> DeltaWorld f () a
closeAppointment i x w = onIndex i (w ^. appointments) $ \y ->
    Right $ (over appointments (fromJust . delete i) . over visits (put i $ x y) $ w, ())



