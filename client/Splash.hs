
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


module Splash where

import Reflex.Dom
import Control.Monad.Reader
import UI.Lib
import Data.String.Here
import HList
import Data.Text (pack)

explanation :: String = [here|
If you want to sell your time or buy other one's time , this is your service!
|]

splash :: (MS m, MonadReader (DS r) m, In Bool r) => m ()
splash = divClass "splash" $ do
    divClass "title" $ text "Book a Visit"
    let f True = divClass "explanation" . text .  pack $ explanation
        f False = return ()
    t <- ask
    dyn $ f <$> see <$> t
    return ()





