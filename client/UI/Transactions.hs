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

data Color = Abortable | Acceptable | WaitingColor | ServingColor | ReleasingColor | GoneColor deriving Show

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
    -> (Color, m(ES (World a -> Ctx OtherT a (World a)))) -- ^ World modification event
transaction (ETaker u) (TTaker i x@(Proposal p)) = (Abortable, aborting i p u)

transaction (EGiver u) (TGiver i x@(Proposal p)) = (Abortable, aborting i p u)

transaction (EGiver u) (TTaker i x@(Proposal p)) =  (Acceptable, accepting i p u x ETaker)
transaction (ETaker u) (TGiver i x@(Proposal p)) = (Acceptable, accepting i p u x EGiver)

transaction me@(EGiver u) (TAbsent i x@(Waiting p)) = (WaitingColor, chatter i x me u ChatWaiting )
transaction me@(EGiver u) (TAbsent i x@(ChattingWaiting p _)) = (WaitingColor, chatter i x me u ChatWaiting)

transaction me@(ETaker u) (TAbsent i x@(Waiting p)) = (WaitingColor, chatter i x me u ChatWaiting )
transaction me@(ETaker u) (TAbsent i x@(ChattingWaiting p _)) = (WaitingColor, chatter i x me u ChatWaiting)

transaction me@(EGiver u) (TAbsent i x@(Serving p)) = (ServingColor, chatter i x me u ChatServing)
transaction me@(EGiver u) (TAbsent i x@(ChattingServing p _)) = (ServingColor, chatter i x me u ChatServing)

transaction me@(ETaker u) (TAbsent i x@(Serving p)) = (ServingColor, chatter i x me u ChatServing)
transaction me@(ETaker u) (TAbsent i x@(ChattingServing p _)) = (ServingColor, chatter i x me u ChatServing)

transaction me@(EGiver u) (TAbsent i x@(Releasing p)) = (ReleasingColor, chatter i x me u ChatReleasing)
transaction me@(EGiver u) (TAbsent i x@(ChattingReleasing p _)) = (ReleasingColor, chatter i x me u ChatReleasing)

transaction me@(ETaker u) (TAbsent i x@(Releasing p)) = (ReleasingColor, chatter i x me u ChatReleasing)
transaction me@(ETaker u) (TAbsent i x@(ChattingReleasing p _)) = (ReleasingColor, chatter i x me u ChatReleasing)


transaction _ (TAbsent i x@(Aborted p)) = (GoneColor, text "not implemented" >> return never)
transaction _ (TAbsent i x@(Dropped p)) = (GoneColor, text "not implemented">> return never)
transaction _ (TAbsent i x@(Failure p)) = (GoneColor, text "not implemented">> return never)
transaction _ (TAbsent i x@(Successed p f)) = (GoneColor, text "not implemented">> return never)

