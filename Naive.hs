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

instance WorldAccess IntMap where
  data Ix IntMap  = Ix Int

  get (Ix i) m = M.lookup i m
  put (Ix i) x m = M.insert i x m

  insert x m = let k = maybe 0 (fst . fst) $ M.maxViewWithKey m
    in (M.insert k x m, Ix k)

  delete (Ix i) m = case i `M.notMember` m of
                      True -> Nothing
                      False -> Just $ i `M.delete` m

type instance Feedback Naive = String
type instance Failure Naive = String
type instance Chat Naive = String
