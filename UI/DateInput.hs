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
{-# language OverloadedLists #-}
{-# language RecursiveDo #-}
{-# language QuasiQuotes #-}
{-# language TypeInType #-}
{-# language ViewPatterns #-}
{-# language OverloadedLists #-}




-- https://youtu.be/btyhpyJTyXg?list=RDG8yEe55gq2c
--
module UI.DateInput where
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
import Text.Read (readMaybe)
import Text.Printf
import UI.Constraints
import UI.ValueInput
import Data.Time ()

import Instance.Date

data Selection a = Selected a | Back

selectDay :: MS m => m (ES (Selection Day))
selectDay = divClass "select-day" $ el "ul" $ do
  bs <- forM [minBound .. maxBound] $ \d -> el "li" $ do
    fmap (Selected d <$) $ button $ pack $ show d
  b <- fmap (Back <$) $ divClass "back" $ button "back"
  return $ leftmost (b:bs)

times = map (\(x,y) -> Delta (ATime x) (ATime y)) $ zip <*> tail $  [7,7.5..22]
selectATime :: MS m  => m (ES (Selection Delta))
selectATime = divClass "select-time" $ el "ul" $ do
  bs <- forM times $ \d -> el "li" $ do
    fmap (Selected d <$) $ button $ pack $ show d
  b <- fmap (Back <$) $ divClass "back" $ button "back"
  return $ leftmost (b:bs)

data Stage = Closed | Daying | Delting Day | Picked Date

picked (Picked x) = Just x
picked _ = Nothing

pickDate' :: MS m => (Bool,Stage) -> m (ES Stage)
pickDate' (False, Closed) = (Daying <$) <$> icon ["calendar","3x"] "pick a date"
pickDate' (True, Closed) = (Daying <$) <$> icon ["calendar-times-o","3x"] "change the date"
pickDate' (_,Daying) = do
  let f Back = Closed
      f (Selected x) = Delting x
  (f <$>) <$> selectDay

pickDate' (_,Delting x) = do
  let f Back = Daying
      f (Selected y) = Picked (Date x y)

  (f <$>) <$> selectATime
pickDate' (r,_) = pickDate' (r,Closed)

instance HasInput Date where
  getInput = divClass "pick-a-date" $ do
    rec domMorph (\d -> never <$ maybe (return ()) (divClass "picked-date" . text . pack . show) d) d
        r <- holdDyn Closed e
        e <- domMorph pickDate' $ (,) <$> (isJust <$> d) <*> r
        d <- holdDyn Nothing $ Just <$> fmapMaybe picked  e
    return d





