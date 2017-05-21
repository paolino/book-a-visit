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

module Simple where

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
import Instance.Simple

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



