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

import UI.DateInput

camera = elAttr "i" [("class","fa fa-camera-retro")] $ text "fa-camera-retro"
validWhat x | length x > 10 = Just x
            | otherwise = Nothing

openWidget  :: forall a u m . (HasInput (Slot a), ReadersU u a, Enum (Zone u a), Bounded (Zone u a), ShowersU u a,  MS m)
      => Part u a -- who I am
      -> ((Bargain a,Slot a, Zone u a) -> IO (World a))
      -> m (Cable (EitherG () (World a)))
openWidget u step = el "ul" $ do
  bargain <- el "li" $ do
    elAttr "span" [("class","field")] $ text "what"
    valueInput "10 chars, minimum" validWhat

  -- zone <-  el "li" $ valueInput "where" readMaybe
  zone <- el "li" $ do
      elAttr "span" [("class","field")] $ text "where"
      divClass "radiochecks" $ radioChecks [minBound .. maxBound]
  -- time :: DS (Maybe (Slot a)) <-  el "li" $ valueInput "when" readMaybe
  time <- el "li" $ do
    elAttr "span" [("class","field")] $ text "when"
    getInput
  -- camera
  (sub,close) <- el "li" $ do
    sub <- submit (fmap (all id) . sequence $ (isJust <$> time): (isJust <$> zone): [isJust <$> bargain])
    close <- icon ["close","3x"] "abandon"
    return (sub,close)
  return $ merge [
    RightG :=> (pushAlways (\r -> liftIO $ step r) $ ((,,) <$> (fromJust <$> bargain) <*> (fromJust <$> time) <*> (fromJust <$> zone)) `tagPromptlyDyn` sub),
    LeftG :=> close
    ]

open (EGiver u) w = openWidget u (\(b,t,z) -> liftIO $ step (NewI (randomIO, FromGiver u (New b t z))) w)
open (ETaker u) w = openWidget u (\(b,t,z) -> liftIO $ step (NewI (randomIO, FromTaker u (New b t z))) w)

partitionE :: (a -> Bool) -> ES a -> (ES a, ES a)
partitionE f = fanEither . fmap (\x -> if f x then Left x else Right x)

