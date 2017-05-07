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

module Instance.Simple where

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
import UI.Acceptance
import UI.Proposal

import Instance.Date

instance Valid (Zone Giver S) (Place Taker S) where
  valid Anywhere _ = True
  valid Here AtYourWorkshop = True
  valid There AtMyHome = True
  valid _ _ = False

instance Valid (Zone Taker S) (Place Giver S) where
  valid Anywhere _ = True
  valid Here AtYourHome = True
  valid There AtMyWorkshop = True
  valid _ _ = False

data S
data Possess = My | Your

---------- example -------------------------------
type instance Bargain S = String
data instance Part Taker S = Client String deriving (Show,Eq)
data instance Part Giver S = Business String deriving (Show,Eq)
type instance Chat S = String
data instance Zone u S = Here | There | Anywhere deriving (Read, Show, Bounded,Enum)
data instance Place Taker S = AtMyHome | AtYourWorkshop deriving (Read, Show, Bounded,Enum)
data instance Place Giver S = AtMyWorkshop | AtYourHome deriving (Read, Show, Bounded, Enum)
type instance Slot S = Date
-- type instance Time S = (Float)
type instance Failure S = String
type instance Feedback S = String

fromBusiness (Business s) = s
fromClient (Client s) = s

instance SlotMatch S where
  data Time S = T Day ATime
  -- matchLow ((t0 , t1) (T t) = t >= t0
  -- matchHigh (t0 , t1) (T t) = t >= t1



deriving instance Show (World S)



