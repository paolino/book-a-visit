{-# language DataKinds #-}
{-# language TypeFamilies #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
{-# language DeriveDataTypeable #-}
{-# language StandaloneDeriving #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language UndecidableInstances #-}
{-# language Rank2Types #-}
{-# language TemplateHaskell #-}
{-# language ScopedTypeVariables #-}
{-# language ConstraintKinds #-}
{-# language DeriveAnyClass #-}
{-# language OverloadedStrings #-}
{-# language RecursiveDo #-}
{-# language QuasiQuotes #-}
{-# language TypeInType #-}
{-# language ViewPatterns #-}
{-# language OverloadedLists #-}




-- https://youtu.be/btyhpyJTyXg?list=RDG8yEe55gq2c
--
module UI.Acceptance where

import Data.Dependent.Map (DMap,DSum((:=>)), singleton)
import qualified Data.Dependent.Map as DMap
import Data.GADT.Compare (GCompare)
import Data.GADT.Compare.TH
import UI.Lib -- (MS,ES,DS, Reason, domMorph, EitherG(LeftG,RightG), rightG,leftG, Cable,sselect)
import Reflex.Dom hiding (Delete, Insert, Link)
import Data.Bifunctor
import Control.Lens hiding (dropping)
import Data.Data.Lens
import Data.Data
import Data.Typeable
import Control.Lens.TH
import System.Random
import qualified Data.Map as M
import Status
import World
import Data.Text (Text,pack,unpack)
import Data.String.Here
import Data.String
import Control.Monad
import Data.Maybe
import Data.Monoid
import Control.Monad.Trans
import Data.Either
import Text.Read (readMaybe)
import UI.Constraints
import UI.Summary
import UI.ValueInput

data Iconified  = Iconified | Disclosed


class Valid a b where
  valid :: a -> b -> Bool

acceptanceWidget  :: (Valid (Zone u a) (Place (Opponent u ) a), Bounded (Place (Opponent u) a), Enum (Place (Opponent u) a),
                      Bounded (Place u a), Enum (Place u a),Read (Place (Opponent u) a), Showers a, ShowersU u a, SummaryC ('Present u) a, MS m)
                => Transaction ProposalT (Present u) a
                -> (Place (Opponent u) a -> Either Except (World a))
                -> Iconified
                -> m (Cable (EitherG Iconified (World a)))

acceptanceWidget t _ Iconified  = do
  b <- divClass "select" (icon ["check","3x"] "accept")
  showTransaction t
  return $ wire (LeftG :=> Disclosed <$ b)


acceptanceWidget t@(Proposal d) step Disclosed = do
  b <- divClass "abort" (button (pack "close"))
  let f Nothing = el "ul" $ do

          place <- el "li" $ divClass "radiochecks" $ radioChecks $ filter (valid $ d ^. zone) [minBound .. maxBound]
          b <- submit (isJust <$> place)
          return $ Just <$> ((fromJust <$> place) `tagPromptlyDyn` b)
      f (Just e) = do
        divClass "error" $ text $ pack $ show e
        (Nothing <$) <$>  button "got it"
  rec   let   g p = step p
              w' = g <$> fmapMaybe id zm
              cm = leftmost [Just <$> lefting w',Nothing <$ ffilter isNothing zm]
        zm <- domMorph f m
        m <- holdDyn Nothing cm

  return $ merge [LeftG :=> Iconified <$ b, RightG :=> righting w']


righting e = (\(Right x) -> x) <$> ffilter isRight e
lefting e = (\(Left x) -> x) <$> ffilter isLeft e

prenote :: (Valid (Zone (Opponent u) a) (Place u a), Bounded (Place (Opponent u) a), Enum (Place (Opponent u) a),
                      Bounded (Place u a), Enum (Place u a),Show (Zone (Opponent u) a), ShowersU u a, Showers a, Read (Place u a), Reflexive u, SummaryC ('Present (Opponent u)) a, MS m)
        => Part u a
        -> (Idx ProposalT (Present (Opponent u)) -> Place u a -> Either Except (World a))
        -> [(Idx ProposalT (Present (Opponent u)), Transaction ProposalT (Present (Opponent u)) a)]
        -> m (ES (World a))

prenote u step xs = el "ul" $ (fmap leftmost) $ forM xs$ \(i,t) -> el "li" $ do

          rec   ws <- domMorph (acceptanceWidget t $ step i) s
                s <- holdDyn Iconified (pick LeftG ws)

          return $ pick RightG ws


