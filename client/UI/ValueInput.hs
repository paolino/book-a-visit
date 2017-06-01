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

{-# LANGUAGE CPP #-}


-- https://youtu.be/btyhpyJTyXg?list=RDG8yEe55gq2c
--
module UI.ValueInput where
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
import Data.List
import Control.Monad
import Control.Monad.Reader

import UI.Constraints
import HList
white = "place-unselected"
green = "place-selected"

checkColor ::  Int -> DS (Maybe Int) -> DS Text
checkColor n d = let
  f (Just i) = if i == n then green else white
  f Nothing = white
  in f <$> d

radioChecks :: forall a m r. (MS m, HasIcons m a,MonadReader (DS r) m, In Bool r) => [a] -> m (DS (Maybe a))
radioChecks xs = do
  rec   ixe :: ES (Int,a) <- fmap leftmost $ el "ul" $ forM (zip [0 ..] xs) $ \(i,x) ->

                    elDynAttr "li" (fmap (M.singleton  "class") . checkColor i $ fmap fst <$> ixd)  $
                      (fmap $ ((i,x) <$)) $ divClass "radiochecks-icon" $ (x <$) <$> getIcon x

        let f :: (Int,a) -> Maybe (Int,a) -> Maybe (Int,a)
            f (i,x) Nothing = Just (i,x)
            f (i,x) (Just (i',_))
              | i == i' = Nothing
              | otherwise = Just (i,x)

        ixd  <- foldDyn f Nothing $ ixe


  return $ fmap snd <$> ixd

onlength c d Nothing = c
onlength c d (Just xs) = d xs

valueInput :: (MonadReader (DS r) m, In Bool r, MS m) => Text -> (String -> Maybe a) -> m (DS (Maybe a))
valueInput placeholder reads = do
  rec   bg <- holdDyn "invalidInput" $ (onlength "invalidInput" $ const "validInput") <$> valueE
        valueD <- do
                t <- elDynClass "div" bg $ textInput $ def & textInputConfig_attributes .~ constDyn [("placeholder",placeholder)]
                return $ reads <$> unpack <$> view textInput_value t
        let valueE = updated valueD
  holdDyn Nothing  valueE

submit c = do
  let f False = return never
      f True =  icon ["check","3x"] "submit"
  domMorph f c

yesno cy = floater $ do
    sub <- submit cy
    close <- icon ["close","3x"] "abandon"
    return (sub,close)

