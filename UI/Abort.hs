
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
module UI.Abort where

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

data Iconified  = Iconified | Disclosed


abortWidget  :: (Read (Place (Opponent u) a), Showers a, SummaryC ('Present u) a, MS m)
                => Transaction ProposalT (Present u) a
                -> Either Except (World a)
                -> Iconified
                -> m (Cable (EitherG Iconified (World a)))

abortWidget t _ Iconified  = do
  b <- divClass "icon" (button (pack $ show $ summary t))
  return $ wire (LeftG :=> Disclosed <$ b)


abortWidget t step Disclosed = do
  b <- divClass "abort" (button (pack "close"))
  let f Nothing = el "ul" $ do
          divClass "modal" $ text "really want to abort the proposal?"
          el "li" $ do
            b <- button "yes"
            return $ wire (RightG :=> True <$ b)
      f (Just e) = do
        divClass "error" $ text $ pack $ show e
        wire' LeftG <$>  button "got it"
  rec   let
            w' = step <$ pick RightG zm
            cm = leftmost [Just <$> lefting w',Nothing <$ pick LeftG zm]
        zm <- domMorph f m
        m <- holdDyn Nothing cm

  return $ merge [LeftG :=> Iconified <$ b, RightG :=> righting w']


righting e = (\(Right x) -> x) <$> ffilter isRight e
lefting e = (\(Left x) -> x) <$> ffilter isLeft e

abort :: (Showers a, Read (Place u a),  Reflexive u, SummaryC ('Present (Opponent u)) a, MS m)
      => (Idx ProposalT (Present (Opponent u)) -> Either Except (World a))
        -> [(Idx ProposalT (Present (Opponent u)), Transaction ProposalT (Present (Opponent u)) a)]
        -> m (ES (World a))

abort step xs = el "ul" $ (fmap leftmost) $ forM xs$ \(i,t) -> el "li" $ do

          rec   ws <- domMorph (abortWidget t $ step i) s
                s <- holdDyn Iconified (pick LeftG ws)

          return $ pick RightG ws

