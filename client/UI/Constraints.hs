{-# language ConstraintKinds #-}
{-# language TypeFamilies #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language Rank2Types #-}

module UI.Constraints where

import World
import Status
import Data.Default
import UI.Lib

type IconsU m u a = (HasIcons m (Zone u a), HasIcons m (Place (Opponent u) a), Bounded (Place (Opponent u) a), Enum (Place (Opponent u) a))
-- type ReadersU u a = (Read (Zone u a),Bargain a ~ String,Read (Place u a))

-- type Defaults a = (Default (Zone Giver a), Default (Zone Taker a), Default (Slot a),Bargain a ~ String,Default (Place 'Giver a),Default (Place Taker a))

type Both f a = (f Giver a, f Taker a)

class ShowPart a where
  showPart :: forall m .MS m => Roled Part a -> m ()
  showChat :: forall m . MS m => Roled RChat a -> m ()
