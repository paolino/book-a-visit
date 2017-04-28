{-# language TemplateHaskell #-}
{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language StandaloneDeriving #-}
{-# language ConstraintKinds #-}
{-# language Rank2Types #-}
{-# language UndecidableInstances #-}
{-# language TypeFamilies #-}
{-# language FlexibleContexts #-}
module World where

import Interaction
import Locations
import Control.Lens.TH (makeLenses)
import Control.Lens
import Data.Kind
import Valid
import Data.Maybe (fromJust)

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

instance (WorldConstraint Monoid f a) => Monoid (World f a) where
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
newOffer :: (WorldAccess f)
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
dropAny :: (WorldAccess f)
          => Lens' (World f a) (f b)  -- ^ giver/taker offer selector
          -> Ix f  -- ^ index of the offer to be deleted
          -> DeltaWorld f () a -- ^ world modification
dropAny l i w = maybe (Left IndexNotFound) Right $ do
  os <- delete i (w ^. l)
  return $ (l .~ os $ w, ())


-- | book an supply moving it to an appointment
bookOffer :: (Eq (Bargain a),Eq (Slot a),WorldAccess f, Reflexive u , Eq (Roles (Opponent u) a), ZonePlace u a, Eq (Place (Opponent u) a))
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
chatAppointment ::  (WorldAccess f) => Ix f -> (Chat a -> Interaction a -> Interaction a) -> Chat a -> DeltaWorld f () a
chatAppointment i t x w = onIndex i (w ^. appointments) $ \y ->
    Right $ (over appointments (put i $ t x y) $ w, ())

-- | close an appointment with a feedback
closeAppointment :: (WorldAccess f) => Ix f -> (Interaction a -> End a) -> DeltaWorld f () a
closeAppointment i x w = onIndex i (w ^. appointments) $ \y ->
    Right $ (over appointments (fromJust . delete i) . over visits (put i $ x y) $ w, ())



