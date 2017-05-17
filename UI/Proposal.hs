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
{-# language OverloadedLists #-}
{-# language RecursiveDo #-}
{-# language QuasiQuotes #-}
{-# language TypeInType #-}
{-# language ViewPatterns #-}
{-# language OverloadedLists #-}
{-# language NoMonomorphismRestriction #-}


module UI.Proposal where
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
import Text.Printf
import UI.Constraints
import UI.ValueInput
import Instance.Date
import Control.Monad.Reader
import UI.DateInput
import qualified GHCJS.DOM.HTMLInputElement as J
import qualified GHCJS.DOM.Element as J

import HList

validWhat x | length x > 10 = Just x
            | otherwise = Nothing

openWidget  
      :: forall a u m r.  ()
      => ( Enum (Zone u a), Bounded (Zone u a),  Bargain a ~ [Char])
      => (MonadReader (DS r) m, In Bool r, HasIcons m (Zone u a), HasInput m (Slot a), MS m)        
      => Part u a -- who I am
      -> ((Bargain a,Slot a, Zone u a) -> World a)
      -> m (Cable (EitherG () (Slot a, World a)))
openWidget u step = el "ul" $ do
  -- zone <-  el "li" $ valueInput "where" readMaybe
  zone <- el "li" $ do
      elAttr "span" [("class","field")] $ text "where"
      divClass "radiochecks" $ radioChecks $ [minBound .. maxBound]
  -- time :: DS (Maybe (Slot a)) <-  el "li" $ valueInput "when" readMaybe
  time <- el "li" $ do
    elAttr "span" [("class","field")] $ text "when"
    getInput
  -- camera
  bargain <- el "li" $ do
    elAttr "span" [("class","field")] $ text "what"
    valueInput "10 chars, minimum" validWhat

  (sub,close) <- el "li" . floater $ do
    sub <- submit (fmap (all id) . sequence $ (isJust <$> time): (isJust <$> zone): [isJust <$> bargain])
    close <- icon ["close","3x"] "abandon"
    return (sub,close)
  let f b t z = (t, step (b,t,z))
  return $ merge [
    RightG :=> ((f <$> (fromJust <$> bargain) <*> (fromJust <$> time) <*> (fromJust <$> zone)) `tagPromptlyDyn` sub),
    LeftG :=> close
    ]

open (EGiver u) w = do
  n <- liftIO $ randomIO
  (,) n <$> openWidget u (\(b,t,z) -> runIdentity $ step (New (Idx n) b t z) u w)
open (ETaker u) w = do
  n <- liftIO $ randomIO
  (,) n <$> openWidget u (\(b,t,z) -> runIdentity $ step (New (Idx n) b t z) u w)

data Section = ClosedSection | OpenSection
proposalDriver u w = divClass "propose record" $ do
  let f ClosedSection = do
          e <-  floater $ icon ["pencil","3x"] "new proposal"
          return $ wire (LeftG :=> OpenSection <$ e)
      f OpenSection = do
            (i,e) <- open u w
            return $ merge [LeftG :=> ClosedSection <$ pick LeftG e, RightG :=> (\(s,w) -> ((s,i),w)) <$> pick RightG e]

  rec   s <- holdDyn ClosedSection $ pick LeftG e
        e <- domMorph f s
  return $ pick RightG e


