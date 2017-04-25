{-# language TypeFamilies #-}
{-# language TemplateHaskell #-}
{-# language InstanceSigs #-}
{-# language DataKinds#-}
{-# language PolyKinds#-}
{-# language KindSignatures #-}
{-# language GADTs #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language ConstraintKinds #-}
{-# language ExistentialQuantification #-}
{-# language UndecidableInstances #-}
-- {-# language UndecidableSuperClasses #-}
{-# language StandaloneDeriving #-}
{-# language AllowAmbiguousTypes #-}
{-# language ScopedTypeVariables #-}
-- {-# language  #-}
module Model where

import Control.Lens.TH
import Control.Lens
import Data.Bifunctor
import Data.Maybe
import Data.Proxy
import GHC.Base
------------------- lib ---------------

class Valid a where
  valid :: a -> Bool

tbd = error "to be implemented"
------------------------------------

-- | veterinary
data family Vet a

-- | pet owner
data family User a

-- | time span

data family Slot a

class Include a b where
  include :: a -> b -> Bool

data family Place a

data family Zone a

data family Match a

type MatchInterface b = (Include (Match b) b,Monoid (Match b), Eq b)
type CleanInterface b = (Include b b,Monoid b ,Eq b)

-- | constraints for implementations
type Interface a =
  (
  Eq (Place a), -- place in a zone
  Include (Zone a) (Place a), -- place in a zone
  CleanInterface (Slot a),
  CleanInterface (Zone a),
  MatchInterface (Bargain a),
  MatchInterface (User a),
  MatchInterface (Vet a)
  )

-- | kind level Phase for a interaction
data Phase1 = Booking | Booked

data Location (b :: Phase1) (o :: * -> *) a where
  -- | By the structure or home
  Home :: Place a ->  Location b o a
  -- | By the structure or home
  Clinic :: Place a ->  Location b o a
  -- | inside a zone
  HomeNeighbour :: Place a -> Zone a -> Location Booking User a
  -- | inside a zone
  ClinicNeighbour :: Place a -> Zone a -> Location Booking Vet a

instance Interface a => Include (Zone a) (Location b o a) where
  include z (Home p) = z `include` p
  include z (Clinic p) = z `include` p
  include z (HomeNeighbour _ p) = z `include` p
  include z (ClinicNeighbour _ p) = z `include` p

type family Opponent (a :: * -> *) where
  Opponent Vet = User
  Opponent User = Vet

-- | a location inside a zone
-- | valid location phase transition Booking -> Booked
instance (o' ~ Opponent o, Interface a) => Valid (Location Booking o a, Location Booked o' a) where
  valid (Home p, Home p') = p == p'
  valid (HomeNeighbour _ z, Clinic p) = z `include` p
  valid (HomeNeighbour p _, Home p') = p == p'
  valid (Clinic p,Clinic p') = p == p'
  valid (ClinicNeighbour _ z, Home p) = z `include` p
  valid (ClinicNeighbour p _, Clinic p') = p == p'

type Showers a =
  (   Show (Chat a)
  ,   Show (Vet a)
  ,   Show (User a)
  ,   Show (Feedback a)
  ,   Show (Failure a)
  ,   Show (Place a)
  ,   Show (Zone a)
  ,   Show (Slot a)
  ,   Show (Match (User a))
  ,   Show (Match (Vet a))
  ,   Show (Match (Bargain a))
  ,   Show (Bargain a)
  )

deriving instance (Showers a) => Show (Location b o a)

-- | informal comunication
type family Chat a

-- | final user expression
type family Feedback a

-- | final alternative vet act
type family Failure a

-- | core semantic for service
type family Bargain a

data Offer u a = Offer {
  _bargain :: Bargain a ,
  _time :: Slot a ,
  _proponent :: u a
                       }
deriving instance (Interface a, Eq (u a)) => Eq (Offer u a)

deriving instance (Showers a, Show (u a)) => Show (Offer u a)

-- opening phase, simmetric
data Dating (u :: * -> *) (b :: Phase1) a where
  -- | a vet or user making an offer
  Open :: Offer u a -> Location Booking u a -> Dating u Booking a
  -- | an opponent accepting an offer
  Appointment :: Offer (Opponent u) a -> u a -> Location Booked u a -> Dating u Booked a
    -- | a user making an offer

deriving instance (Showers a, Show (u a), Show (Opponent u a)) => Show (Dating u b a)

data Phase2 = Waiting | Due

-- | Interaction phase, after boxing the dating phase, parts chat and conclude
data Interaction (b :: Phase2) a where
  BootUser :: Dating User Booked a -> Interaction Waiting a
  BootVet :: Dating Vet Booked a -> Interaction Waiting a
  Chat ::  Either (User a) (Vet a) -> Chat a -> Interaction Waiting a -> Interaction Waiting a
  -- | a succesfully completed interaction
  Closed :: Either (Feedback a) (Failure a) -> Interaction Waiting a ->  Interaction Due a
  -- | premature consensual end
  Abandon :: Interaction Waiting a -> Interaction Due a

deriving instance Showers a => Show (Interaction b a)

instance (Interface a,u' ~ Opponent u, u ~ Opponent u', Eq (u' a)) => Valid (Dating u Booked a, Dating u' Booking a) where
  valid (Appointment a' _ l' , Open a l ) = a == a'  && valid (l,l')



-- | subsetting the world
data Query a = Query
  {   _timeWindow :: Slot a -- ^ time selection
  ,   _spaceWindow :: Zone a -- ^ location selection
  ,   _userWindow :: Match (User a) -- ^ user selection
  ,   _vetWindow  :: Match (Vet a) -- ^ vet selection
  ,   _bargainWindow :: Match (Bargain a)
  }

deriving instance (Showers a) => Show (Query a)

instance Interface a => Monoid (Query a) where
  Query a b c d e `mappend` Query a' b' c' d' e' = Query
    (a `mappend` a')
    (b `mappend` b')
    (c `mappend` c')
    (d `mappend` d')
    (e `mappend` e')
  mempty = Query mempty mempty mempty mempty mempty

data Unmatching = UnmatchedTime | UnmatchedPlace | UnmatchedUser |
    UnmatchedVet | UnmatchedBargain deriving Show

encodeUnmatching x y e = if x `include` y then Right () else Left e

class  Checker b a where
  check :: Query a -> b a ->  Either Unmatching ()

instance Interface a => Checker (Dating Vet b) a where
  check (Query s z u v b) (Open (Offer b' s' v') l ) = do
    encodeUnmatching z l UnmatchedPlace
    encodeUnmatching v v' UnmatchedVet
    encodeUnmatching s s' UnmatchedTime
    encodeUnmatching b b' UnmatchedBargain
  check q@(Query s z u v b) (Appointment (Offer b' s' u') v' l) = do
    encodeUnmatching z l UnmatchedPlace
    encodeUnmatching v v' UnmatchedVet
    encodeUnmatching u u' UnmatchedUser
    encodeUnmatching s s' UnmatchedTime
    encodeUnmatching b b' UnmatchedBargain

instance Interface a => Checker (Dating User b) a where
  check (Query s z u v b) (Open (Offer b' s' u') l) = do
    encodeUnmatching z l UnmatchedPlace
    encodeUnmatching u u' UnmatchedUser
    encodeUnmatching s s' UnmatchedTime
    encodeUnmatching b b' UnmatchedBargain
  check q@(Query s z u v b) (Appointment (Offer b' s' v') u' l) = do
    encodeUnmatching z l UnmatchedPlace
    encodeUnmatching v v' UnmatchedVet
    encodeUnmatching u u' UnmatchedUser
    encodeUnmatching s s' UnmatchedTime
    encodeUnmatching b b' UnmatchedBargain

instance Interface a => Checker (Interaction b) a where
  check q (BootUser d) = check q d
  check q (BootVet d) = check q d
  check q (Chat _ _ d) = check q d
  check q (Closed _ d) = check q d
  check q (Abandon d) = check q d


data World f a = World
  {   _vetOffers :: f (Dating Vet Booking a)
  ,   _userOffers :: f (Dating User Booking a)
  ,   _appointments :: f (Interaction Waiting a)
  ,   _visits :: f (Interaction Due a)
  ,   _select :: Maybe (Query a) -- ^ why this is the world
  }

type Cf (m :: * -> Constraint) f a = (m (f (Dating Vet Booking a)), m (f (Dating User Booking a)),
  m (f (Interaction Waiting a)), m (f (Interaction Due a)))

deriving instance (Showers a, Cf Show f a) => Show (World f  a)

instance (Interface a , Cf Monoid f a ) => Monoid (World f a) where
  mempty = World mempty mempty mempty mempty Nothing
  World s a v f q `mappend` World s' a' v' f' q' =
    World   (s `mappend` s')
            (a `mappend` a')
            (v `mappend` v')
            (f `mappend` f')
            (q `mappend` q')



makeLenses ''World

class Modify f where
  data Ix f
  get :: Ix f -> f b -> Maybe b
  insert :: b -> f b -> (f b, Ix f)
  delete :: Ix f  -> f b -> Maybe (f b)
  -- asList :: f b -> [(Ix f, b)]

data Problem = IndexNotFound | Unmatched Unmatching | InvalidLocationSelection | WrongTransition deriving Show

type DeltaWorld f a = World f a -> Either Problem (World f a)

    {-
-- | start a new supply
newSupply :: (Interface a, Modify f) => Offer Vet a -> Location Booking a -> DeltaWorld f a
newSupply v l s w = let
  o = Supply v l s in
  first Unmatched $ over supplies (fst . insert o) w <$
    maybe (Right ()) (flip check o) (w ^. select)

-- | drop an unbooked supply
dropSupply :: (Interface a, Modify f) => Ix f -> DeltaWorld f a
dropSupply i w = case delete i (w ^. supplies) of
                  Nothing -> Left IndexNotFound
                  Just os -> Right $ supplies .~ os $ w

-- | book a supply moving it to an appointment
bookSupply :: (Interface a, Modify f) => Ix f -> User a -> Location Booked a -> DeltaWorld f a
bookSupply i u l w = case get i (w ^. supplies)  of
                       Nothing -> Left IndexNotFound
                       Just x@(Supply v l' s) -> let
                            a = Appointment u (Supply v l s)
                            in case valid (a,x) of
                                True -> Right . over supplies (fromJust . delete i) . over appointments (fst . insert a) $ w
                                False -> Left InvalidLocationSelection


-}
