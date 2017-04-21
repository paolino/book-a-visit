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

module Naive where

import Model
import qualified Data.Set as S
import Data.Set (Set)
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Spans
import Geometry

data Naive

data instance User Naive = User String deriving (Eq,Show)
data instance Vet Naive = Vet String deriving (Eq,Show)

type Time = Int

data instance Slot Naive = Slot (IntMap Time)  deriving Eq

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

data instance Place Naive = Place Pos deriving Eq
data instance Zone Naive = Zone Pos Distance | NullZone deriving Eq

instance Include (Zone Naive) (Zone Naive) where
  Zone p d `include` Zone p' d' = d > d' && distance p p' < d
  x `include` NullZone = True
  NullZone `include` _ = False

instance Include (Zone Naive) (Place Naive) where
  Zone p d `include` Place p' = distance p p' < d
  NullZone `include` _ = False

instance Monoid (Zone Naive) where
  mempty = NullZone
  NullZone `mappend` x = x
  x `mappend` NullZone = x
  Zone p d `mappend` Zone p' d' = uncurry Zone $ encloser (p,d) (p',d')

instance Include (Slot Naive) (Slot Naive) where
  x `include` y  = (x `mappend` y) == x

data instance Match (User Naive) = MatchUser (Set String)
data instance Match (Vet Naive) = MatchVet (Set String)

instance Include (Match (User Naive)) (User Naive) where
  MatchUser x `include` User y = y `S.member` x

instance Include (Match (Vet Naive)) (Vet Naive) where
  MatchVet x `include` Vet y = y `S.member` x

instance Monoid (Match (User Naive)) where
  MatchUser x `mappend` MatchUser y = MatchUser $ x `mappend` y
  mempty = MatchUser mempty

instance Monoid (Match (Vet Naive)) where
  MatchVet x `mappend` MatchVet y = MatchVet $ x `mappend` y
  mempty = MatchVet mempty

instance Modify IntMap where
  data Ix IntMap  = Ix Int

  get (Ix i) m = M.lookup i m

  insert x m = let k = maybe 0 (fst . fst) $ M.maxViewWithKey m
    in (M.insert k x m, Ix k)

  delete (Ix i) m = case i `M.notMember` m of
                      True -> Nothing
                      False -> Just $ i `M.delete` m
