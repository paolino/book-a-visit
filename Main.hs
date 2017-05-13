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
import Data.List
import Data.Ord
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
import UI.FakeLogin
import UI.Constraints
import Instance.Simple
import UI.ValueInput
import Control.Monad.Reader

import UI.Transactions

css = [hereFile|style.css|]


checkProponent :: Eq (Part u a)  => Part u a -> Transaction s (Present u) a -> Bool
checkProponent u (Proposal t)  = t ^. proponent == u

involved (ETaker u) (ETaker s) = s ^. proposal . proponent == u
involved (ETaker u) (EGiver s) = s ^? acceptance . _Just . accepter == Just u
involved (EGiver u) (EGiver s) = s ^. proposal . proponent == u
involved (EGiver u) (ETaker s) = s ^? acceptance . _Just . accepter == Just u




displayWorld w = do
  el "h3" $ text "Your world view"
  divClass "small" $ dynText $ pack <$> show <$> w

-- main = mainWidgetWithCss (fromString css) $ do
main = mainWidget $ do
    -- elAttr "link" [("href","style.css"),("type","text/css"),("rel","stylesheet")] $ return ()
    t <- divClass "info" $ runReaderT (icon ["question","2x"] "info") (constDyn False)
    d <- foldDyn (const not) False t
    flip runReaderT d $ do
      u <- fakeLogin

      let f (Nothing,w) = divClass "nologin" (divClass "splash" $ text "Book a Visit") >> return never
          f (Just u,w) = divClass "yeslogin" $ do
                            wo <- proposalDriver u w
                            wt <- leftmost <$> mapM (\x -> transaction u x w) (state w)
                            return $ leftmost [wo,wt]

      rec   w :: DS (World S) <- holdDyn mempty dw
            dw <- domMorph f $ (,) <$> u <*> w


{-
    el "hr" $ return ()
    let sw False = (True <$) <$> button "show-world"
        sw True = do
            displayWorld w
            (False <$) <$> button "hide-world"

    rec s <- domMorph sw r
        r <- holdDyn False s
-}
      return ()

