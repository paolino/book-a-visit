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
{-# language DeriveAnyClass #-}
{-# language StandaloneDeriving #-}

module Naive where

import qualified Data.Set as S
import Data.Set (Set)
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Spans
import Geometry
import Locations
import Interaction
import Inclusion
import World

data Naive

data instance Roles b Naive where
  TakerR :: String -> Roles Taker Naive
  GiverR :: String -> Roles Giver Naive

deriving instance Show (Roles b Naive)
deriving instance Eq (Roles b Naive)

type Time = Int

data instance Slot Naive = Slot (IntMap Time)  deriving (Show,Eq)

toSpans :: IntMap Time -> Spans Time
toSpans = map (uncurry Span) . M.assocs

fromSpans :: Spans Time -> IntMap Time
fromSpans = M.fromList . map (\(Span x y) -> (x,y))

instance Monoid (Slot Naive) where
  mempty = Slot mempty
  Slot t  `mappend` Slot t' = Slot . fromSpans . reduce $ toSpans t ++ toSpans t'

type Pos = (Float,Float)
type Distance = Float

distance (x,y) (x',y') = sqrt ((x - x') ^ 2 + (y - y') ^ 2)

data instance Place b Naive = Place Pos deriving (Show,Eq)
data instance Zone b Naive = Zone Pos Distance | NullZone deriving (Show,Eq)

deriving instance Eq (Open Giver Naive)
deriving instance Eq (Open Taker Naive)

instance Include (Zone b Naive) where
  type Target (Zone b Naive) = Place b Naive
  Zone p d `include` Place p' = distance p p' < d
  NullZone `include` _ = False

instance Monoid (Zone b Naive) where
  mempty = NullZone
  NullZone `mappend` x = x
  x `mappend` NullZone = x
  Zone p d `mappend` Zone p' d' = uncurry Zone $ encloser (p,d) (p',d')

instance Include (Slot Naive) where
  type Target (Slot Naive) = Slot Naive
  x `include` y  = (x `mappend` y) == x

newtype IntKMap k a = IntKMap (IntMap a) deriving (Show,Eq)

instance Monoid (IntKMap k a) where
  mempty = IntKMap mempty
  IntKMap x `mappend` IntKMap y = IntKMap $ x `mappend` y

deriving instance Show (Ix k)

instance WorldAccess k IntKMap where
  data Ix k  = Ix Int

  get (Ix i) (IntKMap m) = M.lookup i m
  put (Ix i) x (IntKMap m) = IntKMap $ M.insert i x m

  insert x (IntKMap m) = let k = maybe 0 (fst . fst) $ M.maxViewWithKey m
                          in (IntKMap $ M.insert k x m, Ix k)

  delete (Ix i) (IntKMap m) = IntKMap <$> case i `M.notMember` m of
                      True -> Nothing
                      False -> Just $ i `M.delete` m

type instance Feedback Naive = String
type instance Failure Naive = String
type instance Chat Naive = String
type instance Bargain Naive = String

naiveWorld :: World IntKMap Naive
naiveWorld = mempty

giverBooking :: Ix TakerI -> Roles Giver Naive -> Acceptance Giver Naive -> DeltaWorld IntKMap () Naive
giverBooking = bookOffer takerOffers FromGiver (\(Ix k) -> Ix k)

takerBooking :: Ix GiverI -> Roles Taker Naive -> Acceptance Taker Naive -> DeltaWorld IntKMap () Naive
takerBooking = bookOffer giverOffers FromTaker (\(Ix k) -> Ix k)

giverChat, takerChat :: Ix AnyI -> String -> DeltaWorld IntKMap () Naive
giverChat i = chatAppointment i ChatGiver
takerChat i = chatAppointment i ChatTaker
