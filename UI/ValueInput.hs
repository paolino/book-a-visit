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

import UI.Constraints



radioChecks :: (MS m, Show a) => [a] -> m (DS (Maybe a))
radioChecks xs = do
  rec  (leftmost -> (v :: ES Int), ds) <- el "ul" $ fmap unzip .  forM (zip [0..] xs) $ \(i,x) -> el "li" $ do

                c <- fmap value $ checkbox False $ def  & checkboxConfig_setValue .~ (fmap $ (==) i) v
                divClass "radiochecksText" $ text $ pack $ show $ x
                return ((i <$) . ffilter id . updated $ c, (\y -> if y then Just x else Nothing) <$> c)

  return $ fmap msum $ sequence ds

onlength c d Nothing = c
onlength c d (Just xs) = d xs

valueInput :: (MS m) => Text -> (String -> Maybe a) -> m (DS (Maybe a))
valueInput placeholder reads = do
  rec   bg <- holdDyn "invalidInput" $ (onlength "invalidInput" $ const "validInput") <$> valueE
        valueD <- do
                t <- elDynClass "div" bg $ textInput $ def & textInputConfig_attributes .~ constDyn [("placeholder",placeholder)]
                return $ reads <$> unpack <$> view textInput_value t
        let valueE = updated valueD
  holdDyn Nothing  valueE

submit c = do
  let f False = return never
      f True =  button "submit"
  domMorph f c


