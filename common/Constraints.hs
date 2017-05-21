
{-# language ConstraintKinds #-}
{-# language TypeFamilies #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language Rank2Types #-}

module Constraints where

import Status

type Showers a = ( Show (Zone 'Giver a), Show (Zone Taker a),Show (Part Giver a) ,
  Show (Part Taker a), Show (Slot a), Bargain a ~ String,  Show (Feedback a),
                 Show (Place Giver a), Show (Place Taker a), Show (Chat a))



type ShowersU u a = (Show (Zone u a),  Show (Part u a), Show (Place u a), Show (Place (Opponent u) a), Show (Part (Opponent u) a))

type Eqs0 k (u :: Role) a = (Eq (k u a))
type Eqs u a = Eqs0 Part u a



