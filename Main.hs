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
import UI.Acceptance
import UI.Proposal
import UI.Abort
import UI.FakeLogin
import UI.Constraints
import UI.Summary
import Instance.Simple


css = [hereFile|style.css|]


checkProponent :: Eq (Part u a)  => Part u a -> Transaction s (Present u) a -> Bool
checkProponent u (Proposal t)  = t ^. proponent == u

involved (ETaker u) (ETaker s) = s ^. proposal . proponent == u
involved (ETaker u) (EGiver s) = s ^? acceptance . _Just . accepter == Just u
involved (EGiver u) (EGiver s) = s ^. proposal . proponent == u
involved (EGiver u) (ETaker s) = s ^? acceptance . _Just . accepter == Just u



abortDriver :: (Showers a,Readers a, SlotMatch a, Eq (Part Taker a), Eq (Part Giver a), MS m) => Roled Part a -> World a -> m (ES (World a))
abortDriver u w = divClass "abort" $ case u of
      ETaker u -> case M.assocs . M.filter (checkProponent u) $ w ^. proposalTaker of
                          [] -> return never
                          xs -> do
                              el "h3" $ text "Proposals you can abort"
                              abort (\i -> step (OtherI (FromTaker u (Abort i))) w) xs

      EGiver u -> case M.assocs . M.filter (checkProponent u) $ w ^. proposalGiver of
                          [] -> return never
                          xs -> do
                              el "h3" $ text "Proposals you can abort"
                              abort (\i -> step (OtherI (FromGiver u (Abort i))) w) xs

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

finals :: (Showers a,Readers a, SlotMatch a, Eq (Part Taker a), Eq (Part Giver a), MS m) => Roled Part a ->  World a -> m ()
finals u w = case filter (involved u . summary) . M.elems $ w ^. final of
               [] -> return ()
               xs -> do
                  el "h3" $ text "Gone transactions"
                  divClass "finals" $ forM_ xs $ \t -> case summary t of
                          EGiver t -> showSummary t
                          ETaker t -> showSummary t


displayWorld w = do
  el "h3" $ text "Your world view"
  divClass "small" $ dynText $ pack <$> show <$> w

main = mainWidgetWithCss (fromString css) $ do

    u <- fakeLogin

    let f (Nothing,w) = divClass "nologin" (text "no authentication, no authorization") >> return never
        f (Just u,w) = divClass "logged" $ do
          wo <- proposalDriver u w
          wp <- acceptanceDriver u w
          wa <- abortDriver u w
          finals u w

          return $ leftmost [wo,wp,wa]

    rec   w :: DS (World S) <- holdDyn mempty dw
          dw <- domMorph f $ (,) <$> u <*> w


    el "hr" $ return ()
    let sw False = (True <$) <$> button "show-world"
        sw True = do
            displayWorld w
            (False <$) <$> button "hide-world"

    rec s <- domMorph sw r
        r <- holdDyn False s

    return ()

