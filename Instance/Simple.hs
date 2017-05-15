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
--import UI.Acceptance
--iimport UI.Proposal
import UI.Constraints
import UI.Lib
import Instance.Date
import Reflex.Dom hiding (Abort)
import Control.Monad.Reader
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
{-
-}
data S

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
type instance Feedback S = String

fromBusiness (Business s) = s
fromClient (Client s) = s

instance ShowPart S where
  showPart (ETaker (Client p)) = do
      elAttr "span" [("class","role")] $ text "taker"
      elAttr "span" [("class","name")] $ text $ pack p

  showPart (EGiver (Business p)) = do
      elAttr "span" [("class","role")] $ text "giver"
      elAttr "span" [("class","name")] $ text $ pack p

  showChat (ETaker (RChat p)) = do
      elAttr "span" [("class","role")] $ text "taker"
      elAttr "span" [("class","message")] $ text $ pack p

  showChat (EGiver (RChat p)) = do
      elAttr "span" [("class","role")] $ text "giver"
      elAttr "span" [("class","message")] $ text $ pack p

ihome, iworkshop ::( MonadReader (DS Bool) m, MS m) => m (ES ())
ihome = icon ["home","3x"] "home"
iworkshop = icon ["building-o","3x"] "office"

instance (MonadReader (DS Bool) m,MS m) => HasIcons m (Zone Taker S) where
  getIcon Here =  ihome
  getIcon There =  iworkshop
  getIcon Anywhere =  composeIcon (getIcon (Here :: Zone Taker S)) (getIcon (There:: Zone Taker S))

instance (MonadReader (DS Bool) m,MS m) => HasIcons m (Zone Giver S) where
  getIcon Here =   iworkshop
  getIcon There =  ihome
  getIcon Anywhere = composeIcon (getIcon (Here :: Zone Giver S)) (getIcon (There :: Zone Giver S))

instance (MonadReader (DS Bool) m,MS m) => HasIcons  m (Place Taker S) where
  getIcon AtMyHome = ihome
  getIcon AtYourWorkshop = iworkshop

instance  (MonadReader (DS Bool) m,MS m) => HasIcons m (Place Giver S) where
  getIcon AtYourHome = ihome
  getIcon AtMyWorkshop = iworkshop
deriving instance Show (World S)



