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
module Main where
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
import UI.Acceptance
import UI.Proposal
import UI.FakeLogin
import Instance.Simple


css = [here|
  div.region {
    background:yellow
    }
  div.button.logger {
    color:red
  }
  div.small {
    font-size:80%;
    background:lightgrey;
    margin:2em
  }
  |]


acceptanceDriver u w = divClass "accept" $ case u of
              ETaker u -> case M.assocs $ w ^. proposalGiver of
                          [] -> return never
                          xs -> do
                              el "h3" $ text "Proposals you can take"
                              prenote u (\i p -> step (OtherI (FromTaker u (Appointment i p))) w) xs

              EGiver u ->case M.assocs $ w ^. proposalTaker of
                          [] -> return never
                          xs -> do
                              el "h3" $ text "Proposals you can take"
                              prenote u (\i p  -> step (OtherI (FromGiver u (Appointment i p))) w) xs

proposalDriver u w = divClass "propose" $ do
              el "h3" $ text "Your new proposal"
              open u w


displayWorld w = do
  el "h3" $ text "Your world view"
  divClass "small" $ dynText $ pack <$> show <$> w

main = mainWidgetWithCss (fromString css) $ do

    u <- fakeLogin

    let f (Nothing,w) = divClass "nologin" (text "no authentication, no authorization") >> return never
        f (Just u,w) = divClass "logged" $ do
          wo <- proposalDriver u w
          wp <- acceptanceDriver u w

          divClass "abort" $ do
              el "h3" $ text "Proposals you can abort"

          return $ leftmost [wo,wp]

    rec   w :: DS (World S) <- holdDyn mempty dw
          dw <- domMorph f $ (,) <$> u <*> w

    displayWorld w


