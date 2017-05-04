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
import Lib -- (MS,ES,DS, Reason, domMorph, EitherG(LeftG,RightG), rightG,leftG, Cable,sselect)
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

type Readers a = (Read (Zone Giver a), Read (Zone Taker a), Read (Slot a),Bargain a ~ String)

open  :: forall a m . (Readers a, MS m)
      => Roled Part a -- who I am
      -> World a
      -> m (ES (World a))
open u w = el "ul" $ do
  bargain :: DS String <- el "li" $ do
          text "bargain: "
          fmap unpack <$> view textInput_value <$> textInput def

  zone :: DS String <- el "li" $ do
          text "zone: "
          fmap unpack <$> view textInput_value <$> textInput def

  time <- el "li" $ do
          text "time: "
          fmap unpack <$> view textInput_value <$> textInput def

  submit <- el "li" $ do
          button "submit"
  -- let tagger e = New <$> bargain <*> (read <$> time) <*> (read <$> zone)
  let (e :: ES (Reason NewT a))  = case u of
          EGiver u -> fmap (FromGiver u) $ (New <$> bargain <*> (read <$> time) <*> (read <$> zone)) `tagPromptlyDyn` submit
          ETaker u -> fmap (FromTaker u) $ (New <$> bargain <*> (read <$> time) <*> (read <$> zone)) `tagPromptlyDyn` submit

  return $ pushAlways (\r -> liftIO $ step (NewI (randomIO, r)) w) e


