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


{-# language Rank2Types #-} -- BookingLens definition

module Model where

import Control.Lens.TH (makeLenses)
import Control.Lens ((^.),(.~), Lens',over, set)
import Data.Bifunctor (first)
import Data.Maybe (fromJust)
import GHC.Base (Constraint)
------------------- lib ---------------

class Valid a where
  valid :: a -> Bool

tbd = error "to be implemented"
------------------------------------

-- | givererinary
data family Giver a

-- | pet owner
data family Taker a

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
  MatchInterface (Taker a),
  MatchInterface (Giver a)
  )

-- | kind level Phase for a interaction
data Phase1 = Booking | Booked

data Location (b :: Phase1) (o :: * -> *) a where
  -- | By the structure or home
  Home :: Place a ->  Location b o a
  -- | By the structure or home
  Clinic :: Place a ->  Location b o a
  -- | inside a zone
  HomeNeighbour :: Place a -> Zone a -> Location Booking Taker a
  -- | inside a zone
  ClinicNeighbour :: Place a -> Zone a -> Location Booking Giver a

instance Interface a => Include (Zone a) (Location b o a) where
  include z (Home p) = z `include` p
  include z (Clinic p) = z `include` p
  include z (HomeNeighbour _ p) = z `include` p
  include z (ClinicNeighbour _ p) = z `include` p

type family Opponent (a :: * -> *) where
  Opponent Giver = Taker
  Opponent Taker = Giver

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
  ,   Show (Giver a)
  ,   Show (Taker a)
  ,   Show (Feedback a)
  ,   Show (Failure a)
  ,   Show (Place a)
  ,   Show (Zone a)
  ,   Show (Slot a)
  ,   Show (Match (Taker a))
  ,   Show (Match (Giver a))
  ,   Show (Match (Bargain a))
  ,   Show (Bargain a)
  )

deriving instance (Showers a) => Show (Location b o a)

-- | informal comunication
type family Chat a

-- | final taker expression
type family Feedback a

-- | final alternative giver act
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
  -- | a giver or taker making an offer
  Open :: Offer u a -> Location Booking u a -> Dating u Booking a
  -- | an opponent accepting an offer
  Appointment :: Offer (Opponent u) a -> u a -> Location Booked u a -> Dating u Booked a
    -- | a taker making an offer

deriving instance (Showers a, Show (u a), Show (Opponent u a)) => Show (Dating u b a)

data Phase2 = Waiting | Due

type Boxer u a = Dating u 'Booked a -> Interaction 'Waiting a

-- | Interaction phase, after boxing the dating phase, parts chat and conclude
data Interaction (b :: Phase2) a where
  -- | boxing a Dating taker proposed
  FromTaker :: Dating Taker 'Booked a -> Interaction 'Waiting a
  -- | boxing a Dating giver proposed
  FromGiver :: Dating Giver 'Booked a -> Interaction 'Waiting a
  -- | Chatting recursive state
  ChatTaker ::  Chat a -> Interaction Waiting a -> Interaction Waiting a
  -- | Chatting recursive state
  ChatGiver ::  Chat a -> Interaction Waiting a -> Interaction Waiting a
  -- | a succesfully completed interaction
  Closed :: Either (Feedback a) (Failure a) -> Interaction Waiting a ->  Interaction Due a
  -- | premature consensual end
  Abandon :: Interaction Waiting a -> Interaction Due a

deriving instance Showers a => Show (Interaction b a)

instance (Interface a,u' ~ Opponent u, u ~ Opponent u', Eq (u' a)) => Valid (Dating u Booked a, Dating u' Booking a) where
  valid (Appointment a' _ l' , Open a l ) = a == a'  && valid (l,l')



-- | subsetting the world. Conditions are to be considered composed in AND
data Query a = Query
  {   _timeWindow :: Slot a -- ^ time selection
  ,   _spaceWindow :: Zone a -- ^ location selection
  ,   _takerWindow :: Match (Taker a) -- ^ taker selection
  ,   _giverWindow  :: Match (Giver a) -- ^ giver selection
  ,   _bargainWindow :: Match (Bargain a)
  }


deriving instance (Showers a) => Show (Query a)

instance ( Interface a) => Monoid (Query a) where
  Query a b c d e `mappend` Query a' b' c' d' e' = Query
    (a `mappend` a')
    (b `mappend` b')
    (c `mappend` c')
    (d `mappend` d')
    (e `mappend` e')
  mempty = Query mempty mempty mempty mempty mempty

-- | any phase1 or phase2 value should fail with one of this when checked
data Unmatching
  = UnmatchedTime -- ^ appointment out of time span
  | UnmatchedPlace -- ^ out of zone
  | UnmatchedTaker -- ^ wrong taker
  | UnmatchedGiver -- ^ wrong giver
  | UnmatchedBargain -- ^ wrong bargain
  deriving Show

encodeUnmatching x y e = if x `include` y then Right () else Left e

class  Checker b a where
  check :: Query a -> b a ->  Either Unmatching ()

instance ( Interface a) => Checker (Dating Giver b) a where
  check (Query s z u v b) (Open (Offer b' s' v') l ) = do
    encodeUnmatching z l UnmatchedPlace
    encodeUnmatching v v' UnmatchedGiver
    encodeUnmatching s s' UnmatchedTime
    encodeUnmatching b b' UnmatchedBargain
  check q@(Query s z u v b) (Appointment (Offer b' s' u') v' l) = do
    encodeUnmatching z l UnmatchedPlace
    encodeUnmatching v v' UnmatchedGiver
    encodeUnmatching u u' UnmatchedTaker
    encodeUnmatching s s' UnmatchedTime
    encodeUnmatching b b' UnmatchedBargain

instance Interface a => Checker (Dating Taker b) a where
  check (Query s z u v b) (Open (Offer b' s' u') l) = do
    encodeUnmatching z l UnmatchedPlace
    encodeUnmatching u u' UnmatchedTaker
    encodeUnmatching s s' UnmatchedTime
    encodeUnmatching b b' UnmatchedBargain
  check q@(Query s z u v b) (Appointment (Offer b' s' v') u' l) = do
    encodeUnmatching z l UnmatchedPlace
    encodeUnmatching v v' UnmatchedGiver
    encodeUnmatching u u' UnmatchedTaker
    encodeUnmatching s s' UnmatchedTime
    encodeUnmatching b b' UnmatchedBargain

instance Interface a => Checker (Interaction b) a where
  check q (FromTaker d) = check q d
  check q (FromGiver d) = check q d
  check q (ChatTaker _ d) = check q d
  check q (ChatGiver _ d) = check q d
  check q (Closed _ d) = check q d
  check q (Abandon d) = check q d


data World f a = World
  {   _giverOffers :: f (Dating Giver Booking a)
  ,   _takerOffers :: f (Dating Taker Booking a)
  ,   _appointments :: f (Interaction Waiting a)
  ,   _visits :: f (Interaction Due a)
  ,   _select :: Maybe (Query a) -- ^ why this is the world
  }

-- | World field selector for offers, can be giverOffers or takerOffers
type BookingLens f u a =  Lens' (World f a) (f (Dating u Booking a))

-- | Parametric Constraint kind for data accessible World
type WorldConstraint (m :: * -> Constraint) f a =
  (   m (f (Dating Giver Booking a))
  ,   m (f (Dating Taker Booking a))
  ,   m (f (Interaction Waiting a))
  ,   m (f (Interaction Due a))
  )

-----------------------------
-- World instances ----------
-----------------------------
--
deriving instance (Showers a, WorldConstraint Show f a) => Show (World f  a)

instance (Interface a , WorldConstraint Monoid f a ) => Monoid (World f a) where
  mempty = World mempty mempty mempty mempty Nothing
  World s a v f q `mappend` World s' a' v' f' q' =
    World   (s `mappend` s')
            (a `mappend` a')
            (v `mappend` v')
            (f `mappend` f')
            (q `mappend` q')

----------------------------

makeLenses ''World

class WorldAccess f where
  data Ix f -- ^
  get :: Ix f -> f b -> Maybe b
  put :: Ix f -> b -> f b -> f b
  insert :: b -> f b -> (f b, Ix f)
  delete :: Ix f  -> f b -> Maybe (f b)

-- | failing reasons for all transitions
data Problem = IndexNotFound | Unmatched Unmatching | InvalidLocationSelection  deriving Show

-- | A possible World modification with an outcome 'r'
type DeltaWorld f r a = World f a -> Either Problem (World f a, r)

----------------------------
-- functional interface ----
-- -----------------------

-- | insert a new offer
newOffer :: (Checker (Offer u) a, Interface a, WorldAccess f)
         => BookingLens f u a  -- giver/taker selector
         -> Offer u a -- new offer
         -> Location Booking u a -- offer location
         -> DeltaWorld f (Ix f) a -- world modification


newOffer k o l w = let
  x = Open o l
  (t,i) = insert x (w ^. k)
  in
  first Unmatched $ (set k t w,i) <$
    maybe (Right ()) (flip check o) (w ^. select)

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
bookOffer :: (Interface a, WorldAccess f, u ~ Opponent (Opponent u), Eq (Opponent u a))
          => BookingLens f (Opponent u) a -- ^ giver/taker offer selector
          -> Boxer u a  -- ^ boxing to make a Dating an Interaction
          -> Ix f -- ^ index of the offer to be promoted
          -> u a -- ^ accepter
          -> Location Booked u a -- ^ fixing the offer location
          -> DeltaWorld f () a -- ^ world modification
bookOffer k g i u l w = onIndex i (w ^. k)  $ \x@(Open b l') -> let
                            a = Appointment b u l
                            in case valid (a,x) of
                                True -> Right . flip (,) () . over k (fromJust . delete i) . over appointments (put i $ g a) $ w
                                False -> Left InvalidLocationSelection

-- | close an appointment with a feedback
closeAppointment :: (WorldAccess f, Interface a) => Ix f -> Either (Feedback a) (Failure a) -> DeltaWorld f () a
closeAppointment i x w = onIndex i (w ^. appointments) $ \y ->
    Right $ (over appointments (fromJust . delete i) . over visits (put i $ Closed x y) $ w, ())

-- | chat over an appointment
chatAppointment ::  (WorldAccess f, Interface a) => Ix f -> (Chat a -> Interaction Waiting a -> Interaction Waiting a) -> Chat a -> DeltaWorld f () a
chatAppointment i t x w = onIndex i (w ^. appointments) $ \y ->
    Right $ (over appointments (put i $ t x y) $ w, ())



