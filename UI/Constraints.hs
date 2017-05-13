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

type Showers a = (ShowPart a, Show (Zone 'Giver a), Show (Zone Taker a),Show (Part Giver a) ,
  Show (Part Taker a), Show (Slot a), Bargain a ~ String, Show (Failure a), Show (Feedback a),
                 Show (Place Giver a), Show (Place Taker a), Show (Chat a))



type ShowersU u a = (Show (Zone u a),  Show (Part u a), Show (Place u a), Show (Place (Opponent u) a), Show (Part (Opponent u) a))

type Eqs0 k (u :: Role) a = (Eq (k u a))
type Eqs u a = Eqs0 Part u a


type IconsU m u a = (HasIcons m (Zone u a), HasIcons m (Place (Opponent u) a), Bounded (Place (Opponent u) a), Enum (Place (Opponent u) a))
-- type ReadersU u a = (Read (Zone u a),Bargain a ~ String,Read (Place u a))

-- type Defaults a = (Default (Zone Giver a), Default (Zone Taker a), Default (Slot a),Bargain a ~ String,Default (Place 'Giver a),Default (Place Taker a))

type Both f a = (f Giver a, f Taker a)

class ShowPart a where
  showPart :: forall m .MS m => Roled Part a -> m ()
  showChat :: forall m . MS m => Roled RChat a -> m ()
