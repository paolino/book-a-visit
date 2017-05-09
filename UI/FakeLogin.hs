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


module UI.FakeLogin where

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

import Instance.Simple
import UI.Lib
import UI.Constraints
import Control.Monad.Reader
----------------------- fake login --------------------------------------

users = map (ETaker . Client) ["Paolo Veronelli","Patrizio Iezzi", "Mario Rossi"]
vets = map (EGiver . Business) ["Dottor Volatile", "Piero dei Pesci", "Marta dal Gatto"]

buttonPart p = do
  (e, _) <- elAttr' "div" (M.singleton "class" "login-button")  $ showPart p
  return $ domEvent Click e

data Select a = Selected a | Selecting | LoggedOut

fakeLogin :: forall m . (MonadReader (DS Bool) m, MS m) => m (DS (Maybe (Roled Part S)))
fakeLogin = divClass "fakelogin" $ do
  let f :: Select (Roled Part S) -> m (ES (Select (Roled Part S)))
      f LoggedOut = (Selecting <$) <$> divClass "login" (icon ["user","3x"] "login")
      f (Selected u) = do
        divClass "logged" $ showPart u
        (Selecting <$) <$> divClass "logout" (icon ["user","3x"] "logout")
      f Selecting = divClass "selecting" $ (Selected <$>) <$>  leftmost <$> forM (users ++ vets)
        (\u -> divClass "login" $ (u <$) <$> buttonPart u)



  rec change :: ES (Select (Roled Part S)) <- domMorph f login
      login <- holdDyn LoggedOut change

  let selected (Selected x) = Just (Just x)
      selected LoggedOut = Just Nothing
      selected _ = Nothing

  r <- holdDyn Nothing $  fmapMaybe selected change
  return $ r

----------------------------------------------------------------

