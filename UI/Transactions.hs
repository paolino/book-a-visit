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
{-# language PartialTypeSignatures #-}


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

import Data.Maybe
import UI.Chatting


transaction 
    :: (MS m, MonadReader (DS r) m , In Bool r)
    => IconsU m Taker a 
    => IconsU m Giver a 
    => Eqs Taker a
    => Eqs Giver a
    => Bargain a ~ String
    => Show (Slot a)
    => ShowPart a
    => Chat a ~ String
    => Showers a
    => Valid (Zone Giver a) (Place Taker a)
    => Valid (Zone Taker a) (Place Giver a)
    => Roled Part a  -- ^ author
    -> Box a  -- ^ transaction
    -> m (ES (World a -> Ctx OtherT a (World a))) -- ^ World modification event
transaction (ETaker u) (TTaker i x@(Proposal p)) = aborting i p u

transaction (EGiver u) (TGiver i x@(Proposal p)) = aborting i p u

transaction (EGiver u) (TTaker i x@(Proposal p)) =  accepting i p u x ETaker
transaction (ETaker u) (TGiver i x@(Proposal p)) = accepting i p u x EGiver

transaction me@(EGiver u) (TAbsent i x@(Waiting p)) = chatter i x me u ChatWaiting 
transaction me@(EGiver u) (TAbsent i x@(ChattingWaiting p _)) = chatter i x me u ChatWaiting

transaction me@(ETaker u) (TAbsent i x@(Waiting p)) = chatter i x me u ChatWaiting 
transaction me@(ETaker u) (TAbsent i x@(ChattingWaiting p _)) = chatter i x me u ChatWaiting

transaction me@(EGiver u) (TAbsent i x@(Serving p)) = chatter i x me u ChatServing 
transaction me@(EGiver u) (TAbsent i x@(ChattingServing p _)) = chatter i x me u ChatServing

transaction me@(ETaker u) (TAbsent i x@(Serving p)) = chatter i x me u ChatServing 
transaction me@(ETaker u) (TAbsent i x@(ChattingServing p _)) = chatter i x me u ChatServing

transaction me@(EGiver u) (TAbsent i x@(Releasing p)) = chatter i x me u ChatReleasing 
transaction me@(EGiver u) (TAbsent i x@(ChattingReleasing p _)) = chatter i x me u ChatReleasing

transaction me@(ETaker u) (TAbsent i x@(Releasing p)) = chatter i x me u ChatReleasing 
transaction me@(ETaker u) (TAbsent i x@(ChattingReleasing p _)) = chatter i x me u ChatReleasing


transaction _ (TAbsent i x@(Aborted p)) = text "not implemented" >> return never
transaction _ (TAbsent i x@(Dropped p)) = text "not implemented">> return never
transaction _ (TAbsent i x@(Failure p)) = text "not implemented">> return never
transaction _ (TAbsent i x@(Successed p f)) = text "not implemented">> return never

