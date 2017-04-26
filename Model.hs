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
module M2 where

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

-- opening phase, symmetric on roles
data Dating (u :: Role) (b :: Phase) a where
  -- | a giver or taker making an offer
  Open :: Offer u a -> Location Proposal u a -> Dating u Proposal a
  -- | an opponent accepting an offer
  Appointment :: Offer (Opponent u) a -> Roles u a -> Location Acceptance u a -> Dating u Acceptance a
    -- | a taker making an offer

deriving instance (Showers a) => Show (Dating Giver b a)
deriving instance (Showers a) => Show (Dating Taker b a)

data Phase2 = Waiting | Due


-- | Interaction phase, after boxing the dating phase, parts chat and conclude
data Interaction (b :: Phase2) a where
  -- | boxing a Dating taker proposed
  FromTaker :: Dating Taker 'Acceptance a -> Interaction 'Waiting a
  -- | boxing a Dating giver proposed
  FromGiver :: Dating Giver 'Acceptance a -> Interaction 'Waiting a
  -- | Chatting recursive state
  ChatTaker ::  Chat a -> Interaction Waiting a -> Interaction Waiting a
  -- | Chatting recursive state
  ChatGiver ::  Chat a -> Interaction Waiting a -> Interaction Waiting a
  -- | a succesfully completed interaction
  Closed :: Either (Feedback a) (Failure a) -> Interaction Waiting a ->  Interaction Due a
  -- | premature consensual end
  Abandon :: Interaction Waiting a -> Interaction Due a

type Boxer u a = Dating u 'Acceptance a -> Interaction 'Waiting a
deriving instance Showers a => Show (Interaction b a)

instance (Eq (Place u' a), ZonePlace u a, Interface a,Symmetric u u', Eq (Roles u' a)) => Valid (Dating u Acceptance a, Dating u' Proposal a) where
  valid (Appointment a' _ l' , Open a l ) = a == a'  && acceptance l l'



-- | subsetting the world. Conditions are to be considered composed in AND
data Query a = Query
  {   _timeWindow :: Slot a -- ^ time selection
  ,   _takerZoneWindow :: Zone Taker a -- ^ location selection
  ,   _giverZoneWindow :: Zone Giver a -- ^ location selection
  ,   _takerWindow :: Match (Roles Taker a) -- ^ taker selection
  ,   _giverWindow  :: Match (Roles Giver a) -- ^ giver selection
  ,   _bargainWindow :: Match (Bargain a)
  }


deriving instance (Showers a) => Show (Query a)

instance ( Interface a) => Monoid (Query a) where
  Query a b c d e f `mappend` Query a' b' c' d' e' f' = Query
    (a `mappend` a')
    (b `mappend` b')
    (c `mappend` c')
    (d `mappend` d')
    (e `mappend` e')
    (f `mappend` f')

  mempty = Query mempty mempty mempty mempty mempty mempty

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

  check (Query s zt zg u v b) (Open (Offer b' s' v') zg' ) = do
    encodeUnmatching zg zg' UnmatchedPlace
    encodeUnmatching v v' UnmatchedGiver
    encodeUnmatching s s' UnmatchedTime
    encodeUnmatching b b' UnmatchedBargain

  check q@(Query s zt zg u v b) (Appointment (Offer b' s' u') v' zg') = do
    encodeUnmatching zg zg' UnmatchedPlace
    encodeUnmatching v v' UnmatchedGiver
    encodeUnmatching u u' UnmatchedTaker
    encodeUnmatching s s' UnmatchedTime
    encodeUnmatching b b' UnmatchedBargain

{-
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
  {   _giverOffers :: f (Dating Giver Proposal a)
  ,   _takerOffers :: f (Dating Taker Proposal a)
  ,   _appointments :: f (Interaction Waiting a)
  ,   _visits :: f (Interaction Due a)
  ,   _select :: Maybe (Query a) -- ^ why this is the world
  }

-- | World field selector for offers, can be giverOffers or takerOffers
type ProposalLens f u a =  Lens' (World f a) (f (Dating u Proposal a))

-- | Parametric Constraint kind for data accessible World
type WorldConstraint (m :: Type -> Constraint) f a =
  (   m (f (Dating Giver Proposal a))
  ,   m (f (Dating Taker Proposal a))
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
         => ProposalLens f u a  -- giver/taker selector
         -> Offer u a -- new offer
         -> Location Proposal u a -- offer location
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
bookOffer :: (Interface a, WorldAccess f, u ~ Opponent (Opponent u), Eq (Roles (Opponent u) a))
          => ProposalLens f (Opponent u) a -- ^ giver/taker offer selector
          -> Boxer u a  -- ^ boxing to make a Dating an Interaction
          -> Ix f -- ^ index of the offer to be promoted
          -> Roles u a -- ^ accepter
          -> Location Acceptance u a -- ^ fixing the offer location
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


-}
