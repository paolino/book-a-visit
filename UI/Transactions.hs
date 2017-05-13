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
{-# language NoMonomorphismRestriction #-}
{-# language OverloadedLists #-}



module UI.Transactions where

import Reflex hiding (Abort)
import World
import Status
import UI.Lib
import qualified Data.Map as M
import Control.Lens
import Data.List
import Data.Ord
import Control.Monad.Reader
import Control.Arrow
import Reflex.Dom hiding (Abort)
import UI.ValueInput
import UI.Acceptance
import HList
import Data.Text (pack)
import UI.Constraints
import Constraints
check c f = if c then f else return never
transaction 
    :: (MS m, MonadReader (DS r) m , In Bool r)
    => IconsU m Taker a 
    => IconsU m Giver a 
    => Eqs Taker a
    => Eqs Giver a
    => Bargain a ~ String
    => Show (Slot a)
    => ShowPart a
    => Valid (Zone Giver a) (Place Taker a)
    => Valid (Zone Taker a) (Place Giver a)
    => Roled Part a  -- ^ author
    -> Box a  -- ^ transaction
    -> World a 
    -> m (ES (World a)) -- ^ World modification event
transaction (ETaker u) (TTaker i x@(Proposal p)) w = check (p ^. proponent == u) . divClass "record abort" $ do
    el "ul" $ do 
      el "li" $ do
        elAttr "span" [("class","field")] $ text "when"
        divClass "timeshow" $ text $ pack $ show $ p ^. slot
      el "li" $ do
          elAttr "span" [("class","field")] $ text "where"
          divClass "placeshow" $ getIcon $ p ^. zone
      el "li" $ do
        elAttr "span" [("class","field")] $ text "what"
        divClass "bargainshow" $ text $ pack $ p ^. bargain
    abortDriver (step (Abort i) u w) 
transaction (EGiver u) (TGiver i x@(Proposal p)) w = abortDriver  (step (Abort i) u w)
transaction (EGiver u) (TTaker i x@(Proposal p)) w =  divClass "record accept" $ do
    el "ul" $ do 
      el "li" $ do
        elAttr "span" [("class","field")] $ text "when"
        divClass "timeshow" $ text $ pack $ show $ p ^. slot
      el "li" $ do
          elAttr "span" [("class","field")] $ text "where"
          divClass "placeshow" $ getIcon $ p ^. zone
      el "li" $ do
        elAttr "span" [("class","field")] $ text "what"
        divClass "bargainshow" $ text $ pack $ p ^. bargain
      el "li" $ do
        elAttr "span" [("class","field")] $ text "proponent"
        divClass "opponent" $ showPart $ ETaker (p ^. proponent)

      acceptDriver (\ch -> step (Appointment i ch)  u w) x
transaction (ETaker u) (TGiver i x@(Proposal p)) w = acceptDriver (\ch -> step (Appointment i ch)  u w) x
transaction _ _ _ = text "not implemented" >> return never
