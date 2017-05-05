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




-- https://youtu.be/btyhpyJTyXg?list=RDG8yEe55gq2c
--
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


import UI.Constraints
import UI.ValueInput


openWidget  :: (ReadersU u a, MS m)
      => Part u a -- who I am
      -> ((Bargain a,Slot a, Zone u a) -> IO (World a))
      -> m (ES (World a))
openWidget u step = el "ul" $ do
  bargain :: DS String <- el "li" $ do
          text "bargain: "
          fmap unpack <$> view textInput_value <$> textInput def

  zone <-  el "li" $ valueInput "zone"

  time <-  el "li" $ valueInput "time"

  sub <- submit (liftM2 (&&) (isJust <$> time) (isJust <$> zone))

  return $ pushAlways (\r -> liftIO $ step r) $ ((,,) <$> bargain <*> (fromJust <$> time) <*> (fromJust <$> zone)) `tagPromptlyDyn` sub

open (EGiver u) w = openWidget u (\(b,t,z) -> liftIO $ step (NewI (randomIO, FromGiver u (New b t z))) w)
open (ETaker u) w = openWidget u (\(b,t,z) -> liftIO $ step (NewI (randomIO, FromTaker u (New b t z))) w)

partitionE :: (a -> Bool) -> ES a -> (ES a, ES a)
partitionE f = fanEither . fmap (\x -> if f x then Left x else Right x)

