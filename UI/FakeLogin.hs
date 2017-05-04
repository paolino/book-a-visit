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
----------------------- fake login --------------------------------------

users = map (ETaker . Client) ["Paolo Veronelli","Patrizio Iezzi", "Mario Rossi"]
vets = map (EGiver . Business) ["Dottor Volatile", "Piero dei Pesci", "Marta dal Gatto"]

data Select a = Selected a | Selecting

fakeLogin :: MS m => m (DS (Maybe (Roled Part S)))
fakeLogin = do
  let f (Selected u) = (Selecting <$) <$> do
                        divClass "logger" $ button (pack $  maybe "login" show $ bimap  fromBusiness fromClient <$> u)
      f Selecting = (fmap Selected . leftmost) <$> do
        xs <- forM (users ++ vets) $ \u -> divClass "login" $
            (Just u <$) <$> button (pack $ show $ bimap  fromBusiness fromClient u)

        x <- (Nothing <$) <$>  divClass "logger" (button "logout")

        return $ x:xs

  rec change <-domMorph f login
      login <- holdDyn (Selected Nothing) change

  let selected (Selected x) = Just x
      selected _ = Nothing

  r <- holdDyn Nothing $ fmapMaybe selected change
  return $ r

----------------------------------------------------------------

