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




module UI.Transactions where

import Reflex
import World
import Status
import UI.Lib
import qualified Data.Map as M
import Control.Lens
import Data.List
import Data.Ord
import Control.Monad.Reader


type WithLogin r a = Lens' r (DS (Roled Part a))

type Assoc s u a = (Idx s u, Transaction s u a)
class MonadReader r m => TransactionWidget m r s u a where
    widgetOn :: WithLogin r a -> Assoc s u a -> m (ES (World a))


data BoxWidget m r a = forall s u. (SummaryC u a, TransactionWidget m r s u a) => BoxWidget (Assoc s u a)

getSlotBox :: forall a m r. BoxWidget m r a -> Slot a
getSlotBox (BoxWidget (_,x)) = same $ throughSummary (\s -> s ^. proposal . slot) x

type State m r a = [BoxWidget m r a]

fromField :: (SummaryC u a, TransactionWidget m r s u a) =>  MapW s u a -> State m r a
fromField = map BoxWidget . M.assocs

type TWC m r a = (TransactionWidget m r ProposalT ('Present 'Giver) a, 
    TransactionWidget m r ProposalT ('Present 'Taker) a, 
    TransactionWidget m r WaitingT Absent a, 
    TransactionWidget m r ServingT Absent a, 
    TransactionWidget m r ReleasingT Absent a,
    TransactionWidget m r FinalT Absent a)

fromWorld :: (Ord (Slot a), TWC m r a)
    => World a -> State m r a

fromWorld w = sortBy (comparing getSlotBox) $ 
    fromField (w ^. proposalGiver) ++
    fromField (w ^. proposalTaker) ++
    fromField (w ^. waiting) ++
    fromField (w ^. serving) ++
    fromField (w ^. releasing) ++
    fromField (w ^. final)

stepBox :: MonadReader r m => WithLogin r a -> BoxWidget m r a -> m (ES (World a))
stepBox l (BoxWidget x) = widgetOn l x
-- stateW :: World a -> m (DS (World a))

stateW :: (Ord (Slot a), TWC m r a, MS m, MonadReader r m) => WithLogin r a -> World a -> m (DS (World a))
stateW l w0 = do
    rec s <- holdDyn w0 nw
        nw <- domMorph (\xs -> leftmost <$> mapM (stepBox l) xs) (fromWorld <$> s)
    return s

