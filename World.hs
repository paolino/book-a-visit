{-# language TemplateHaskell #-}
{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language StandaloneDeriving #-}
{-# language ConstraintKinds #-}
{-# language Rank2Types #-}
{-# language UndecidableInstances #-}
{-# language TypeFamilies #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds #-}
{-# language FunctionalDependencies #-}

module World where

import Interaction
import Locations
import Control.Lens.TH (makeLenses)
import Control.Lens
import Data.Kind
import Valid
import Data.Maybe (fromJust)

class WorldAccess (k :: ki) f where
  data Ix k -- ^
  get :: Ix k  -> f k b -> Maybe b
  put :: Ix k  -> b -> f k b -> f k b
  insert :: b -> f k b -> (f k b, Ix k )
  delete :: Ix k   -> f k b -> Maybe (f k b)

data Indices = GiverI | TakerI | InteractingI | EndingI


type family FromRoles a where
  FromRoles Giver = GiverI
  FromRoles Taker = TakerI

data World f a = World
  {   _giverOffers :: f GiverI (Open Giver a)
  ,   _takerOffers :: f TakerI (Open Taker a)
  ,   _appointments :: f InteractingI (Interaction a)
  ,   _visits :: f EndingI (End a)
  }

-- | World field selector for offers, can be giverOffers or takerOffers
type ProposalLens f u a =  Lens' (World f a) (f (FromRoles u) (Open u a))

-- | Parametric Constraint kind for data accessible World
type WorldConstraint (m :: Type -> Constraint) f a =
  (   m (f GiverI (Open Giver a))
  ,   m (f TakerI (Open Taker a))
  ,   m (f InteractingI (Interaction a))
  ,   m (f EndingI (End a))
  )

-----------------------------
-- World instances ----------
-----------------------------
--
deriving instance (Showers a, WorldConstraint Show f a) => Show (World f a)
deriving instance ( WorldConstraint Eq f a) => Eq (World f a)

instance (WorldConstraint Monoid f a) => Monoid (World f a) where
  mempty = World mempty mempty mempty mempty
  World s a v f `mappend` World s' a' v' f' =
    World   (s `mappend` s')
            (a `mappend` a')
            (v `mappend` v')
            (f `mappend` f')

----------------------------

makeLenses ''World



-- | failing reasons for all transitions
data Problem = IndexNotFound |  InvalidLocationSelection  deriving (Show,Eq)

-- | A possible World modification with an outcome 'r'
type DeltaWorld f r a = World f a -> Either Problem (World f a, r)

----------------------------
-- functional interface ----
-- -----------------------
type family StepIx a where
  StepIx GiverI = InteractingI
  StepIx TakerI = InteractingI
  StepIx InteractingI = EndingI

type family FromIx a where
  FromIx GiverI = Giver
  FromIx TakerI = Taker

class  Step (a :: Indices) where
  step :: Ix a -> Ix (StepIx a)

data Box f i a = Box {
  blens :: Lens' (World f a) (f i (Open (FromIx i) a)),
  bboxer :: Appointment (Opponent (FromIx i)) a -> Interaction a
  }

class WorldLens f i a  where
  worldLens :: Ix i -> Box f i a

instance WorldLens f GiverI a where
  worldLens _ = Box giverOffers FromTaker

instance WorldLens f TakerI a where
  worldLens _ = Box takerOffers FromGiver


-- | insert a new offer
newOffer :: (WorldAccess (FromRoles u) f, FromIx (FromRoles u) ~ u,WorldLens f (FromRoles u) a)
         => Roles u a -- new offer
         -> Bargain a -- new offer
         -> Slot a -- new offer
         -> Proposal u a -- offer location
         -> DeltaWorld f (Ix (FromRoles u)) a -- world modification


newOffer u b s p w = let
  x = Open u b s p
  (t,i) = insert x (w ^. blens (worldLens i))
  in return $ (set (blens (worldLens i)) t w,i)

onIndex :: WorldAccess k f => Ix k -> f k a -> (a -> Either Problem b) -> Either Problem b
onIndex i xs f = maybe (Left IndexNotFound) f $ get i xs

-- | dropAny index, decide your auth somewhere else
dropAny :: (WorldAccess k f, WorldLens f k a)
  => Ix k  -- ^ index of the offer to be deleted
  -> DeltaWorld f () a -- ^ world modification
dropAny i w = maybe (Left IndexNotFound) Right $ do
  os <- delete i (w ^. blens (worldLens i))
  return $ (blens (worldLens i) .~ os $ w, ())


-- | book an supply moving it to an appointment, for brevity the transphaser is the (Opponent u)
bookOffer :: (Eq (Bargain a),Eq (Slot a),WorldAccess (FromRoles u) f,FromIx (FromRoles u) ~ u, WorldLens f (FromRoles u) a,
              WorldAccess InteractingI f, Reflexive u , Eq (Roles u a), ZonePlace (Opponent u) a, Eq (Place u a),
              StepIx (FromRoles u) ~ 'InteractingI, Step (FromRoles u))
  => Ix (FromRoles u) -- ^ index of the offer to be promoted
  -> Roles (Opponent u) a -- ^ accepter
  -> Acceptance (Opponent u) a -- ^ fixing the offer location
  -> DeltaWorld f () a -- ^ world modification
bookOffer i u l w = let
  Box le g = worldLens i
  in onIndex i (w ^. le)  $ \x -> let
                            a = Appointment x u l
                            in case valid a of
                                True -> Right . flip (,) () . over (blens $ worldLens i) (fromJust . delete i) . over appointments (put (step i) $ g a) $ w
                                False -> Left InvalidLocationSelection

-- | chat over an appointment
chatAppointment ::  (WorldAccess InteractingI f) => Ix InteractingI -> (Interaction a -> Interaction a) -> DeltaWorld f () a
chatAppointment i x w = onIndex i (w ^. appointments) $ \y ->
    Right $ (over appointments (put i $ x y) $ w, ())

-- | close an appointment with a feedback
closeAppointment :: (WorldAccess InteractingI f, WorldAccess EndingI f, Step InteractingI) => Ix InteractingI -> (Interaction a -> End a) -> DeltaWorld f () a
closeAppointment i x w = onIndex i (w ^. appointments) $ \y ->
  Right $ (over appointments (fromJust . delete i) . over visits (put (step i) $ x y) $ w, ())

    {-

-}
