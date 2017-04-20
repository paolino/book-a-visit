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
-- {-# language  #-}
module Model where

import Control.Lens.TH
import Control.Lens
import Data.Bifunctor
import Data.Maybe

------------------- lib ---------------

class Valid a where
  valid :: a -> Bool

tbd = error "to be implemented"
------------------------------------




-- | veterinary
type family Vet a

-- | pet owner
type family User a

-- | absolute time
type family Time a

-- | time span
type family Delta a

-- | time intervals
data Slot a = Slot (Time a) (Delta a)

class Include a b where
  include :: a -> b -> Bool

type family Place a

type family Zone a

type family Match a


type Interface a = (Eq (Vet a), Eq (User a),Eq (Slot a), Eq (Place a),
  Include (Zone a) (Place a),
  Include (Zone a) (Zone a),
  Include (Slot a) (Slot a),
  Include (Match (User a)) (User a),
  Include (Match (Vet a)) (Vet a)
                   )

-- | kind level Phase
data Phase = Booking | Booked | Waiting | Due

data Location (b :: Phase) a where
  -- | By the structure, supplying and taking
  Clinic :: Place a ->  Location b a
  -- | Home supplying, inside a zone
  HomeSupply :: Zone a -> Location Booking a
  -- | Home taking
  Home :: Place a -> Location Booked a
  -- | At clinic or home supplying
  AnySupply :: Place a -> Zone a -> Location Booking a

instance (Include (Zone a) (Zone a), Include (Zone a) (Place a), c ~ Zone a ) => Include c (Location b a) where
  z `include` Clinic p = z `include` p
  z `include` Home p = z `include` p
  z `include` HomeSupply p = z `include` p
  z `include` AnySupply p z' = z `include` p || z `include` z'

instance Interface a => Valid (Location Booking a, Location Booked a) where
  valid (Clinic p, Clinic p') = p == p'
  valid (HomeSupply z, Home p) = z `include` p
  valid (AnySupply p z, Home p') = z `include` p'
  valid (AnySupply p z, Clinic p') = p == p'

type family Feedback a

type family Justification a

data Record (b :: Phase) a where
  Supply :: Vet a -> Location b a -> Slot a -> Record b a
  Appointment :: User a -> Record Booked a -> Record Waiting a
  Visit :: Feedback a -> Record Booked a -> Record Due a
  Failure :: Justification a -> Record Booked a -> Record Due a

instance Interface a => Valid (Record Booked a, Record Booking a) where
  valid (Supply a l d , Supply a' l' d') = a == a' && d == d' && valid (l',l)

instance Interface a => Valid (Record Waiting a, Record Booking a) where
  valid (Appointment _ r, r') = valid (r,r')


-- | subsetting the world
data Query a = Query
  {   _timeWindow :: Slot a -- ^ time selection
  ,   _spaceWindow :: Zone a -- ^ location selection
  ,   _userWindow :: Match (User a) -- ^ user selection
  ,   _vetWindow  :: Match (Vet a) -- ^ vet selection
  }

data Unmatching = UnmatchedTime | UnmatchedPlace | UnmatchedUser | UnmatchedVet

encodeUnmatching x y e = if x `include` y then Right () else Left e

check :: Interface a => Query a -> Record b a ->  Either Unmatching ()
check (Query s z u v) (Supply v' l s') = do
  encodeUnmatching z l UnmatchedPlace
  encodeUnmatching v v' UnmatchedVet
  encodeUnmatching s s' UnmatchedTime

check q@(Query s z u v) (Appointment u' o) = encodeUnmatching u u' UnmatchedUser >> check q o

check q (Visit _ x)  = check q x

check q (Failure _ x) = check q x


type Map f (b :: Phase) a = f (Record b a)

data World f a = World
  {   _supplies :: Map f Booking a
  ,   _appointments :: Map f Waiting a
  ,   _visits :: Map f Due a
  ,   _failures :: Map f Due a
  ,   _select :: Query a -- ^ why this is the world
  }

makeLenses ''World

class Modify f where
  data Ix f
  get :: Ix f -> f b -> Maybe b
  insert :: b -> f b -> (f b, Ix f)
  delete :: Ix f  -> f b -> Maybe (f b)
  -- asList :: f b -> [(Ix f, b)]

data Problem = IndexNotFound | Unmatched Unmatching | InvalidLocationSelection | WrongTransition

type DeltaWorld f a = World f a -> Either Problem (World f a)

-- | start a new supply
newSupply :: (Interface a, Modify f) => Vet a -> Location Booking a -> Slot a -> DeltaWorld f a
newSupply v l s w = let
  o = Supply v l s in
  first Unmatched $ over supplies (fst . insert o) w <$ check (w ^. select) o

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


      {-
data Modification = Book | Report | Blow

type family Transaction (m :: Modification) a

type instance Book a = (Off

data Action a = Insert (Supply Booking a) |  Drop (Supply Booking a)  | Book (Supply Booking a) (

data Select a = Select
  {   _bySlot :: Slot a
  ,   _byVet :: Maybe (Vet a)
  ,   _byUser :: Maybe (User a)
  ,   _byZone :: Maybe (Zone a)
  }

makeLenses ''Visit
makeLenses ''Supply
makePrisms ''Status
makeLenses ''World
makeLenses ''Select



instance Timing a => Valid (Select a, World a) where
  valid (Select





-}
