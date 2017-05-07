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
module UI.Summary  where

import Data.Dependent.Map (DMap,DSum((:=>)), singleton)
import qualified Data.Dependent.Map as DMap
import Data.GADT.Compare (GCompare)
import Data.GADT.Compare.TH
import UI.Lib -- (MS,ES,DS, Reason, domMorph, EitherG(LeftG,RightG), rightG,leftG, Cable,sselect)
import Reflex.Dom hiding (Delete, Insert, Link, Abort)
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


showSummary :: (MS m, Showers a, ShowersU u a) => Summary u a -> m ()
showSummary (Summary p ma cs mo) = divClass "summary" $ el "ul" $ do
  el "li" $ do
    text "bargain: "
    text $ pack $  p ^. bargain


  el "li" $ do
    text "proponent: "
    text $ pack $ show $ p ^. proponent

  case ma of
      Nothing -> return ()
      Just a -> do
            text "accepter: "
            text $ pack $ show $ a ^. accepter

  el "li" $ do
    text "time: "
    text $ pack $ show $ p ^. slot

  el "li" $ do
    text "place: "
    text $ pack $ maybe (show $ p ^. zone) (show . view place) ma

  case cs of
    [] -> return ()
    cs -> divClass "chats" $ el "ul" $ forM_ cs $ \c ->
        el "li" $  text $ pack $ show $ c
  case mo of
    Nothing -> return ()
    Just r -> el "li" $ text $ pack $ show $ r

showTransaction :: (MS m, Showers a, SummaryC u a) => Transaction s u a -> m ()
showTransaction t = case summary t of
                ETaker s -> showSummary s
                EGiver s -> showSummary s

