{-# language TypeFamilies #-} -- free implementations

{-# language GADTs #-}  --  Proposal, Acceptance

{-# language FlexibleContexts #-} -- valid
--
{-# language FlexibleInstances #-} -- valid
{-# language UndecidableInstances #-}

{-# language ConstraintKinds #-} -- synonyms of constraints


{-# language StandaloneDeriving #-} -- show and eq


{-# language PolyKinds #-} -- kinds introduction in type

module Locations where

import GHC.Base (Constraint, Type)
import Inclusion
import Valid


-- | Opponence relation for a kind (ko)
type family Opponent (a :: ka) :: ka

-- | Reflexive constraint for Opponent family
type Reflexive (a :: ka)  =
  Opponent (Opponent a) ~ a

type Symmetric (a :: ka) (b :: ka) = (Opponent a ~ b, Opponent b ~ a)

-- | Symmetric constraint application for Opponent family
type SymmetricC (r :: kr) a (s :: Type -> Constraint) (u :: kr -> Type -> Type) =
  (s (u r a), s (u (Opponent r) a))


-- | values representing a place
data family Place (r :: kr) a

-- | values representing a zone
data family Zone (r :: kr) a

-- | Constraint for Zone and Place
type ZonePlace r a =  (Target (Zone r a) ~ Place r a, Include (Zone r a))


-- | a location parametrized by a role kind (kr)
data Proposal (r :: kr) a where
  -- | Proposal fixed place
  At :: Place r a ->  Proposal r a
  -- | Somewhere in a zone proposal
  Around :: Zone (Opponent r) a ->   Proposal r a
  -- | Somewhere in a zone or fixed place proposal
  AtAround :: Place r a -> Zone (Opponent r) a ->  Proposal r a

-- | acceptance is the act of fixing the place, which has 2 solutions
-- AtProponent is at the proponent place (Opponent r place parameter)
data Acceptance (r :: kr) a where
  -- | Acceptance of the proponent fixed place
  AtProponent :: Place (Opponent r) a ->   Acceptance r a
  -- | Acceptance of accepter fixed place
  AtAccepter :: Place r a ->   Acceptance r a


-- | validity at term level of type level constrained Location change of phase
--
instance (
    Eq (Place r a) -- just for proponent place
    ,   Reflexive r
    ,   ZonePlace (Opponent r) a -- just for accepter zone
         , v' ~ Acceptance (Opponent r) a
         ) => Valid (Proposal r a, v')  where

  valid (At p,AtProponent q) = p == q
  valid (Around z, AtAccepter q) = z `include` q
  valid (AtAround _ z, AtAccepter q) = z `include` q
  valid (AtAround p _, AtProponent q) = p == q


deriving instance
  (   SymmetricC r a Show Place
  ,   SymmetricC r a Show Zone
  ) =>
  Show (Proposal r a)
deriving instance
  (   SymmetricC r a Show Place
  ,   SymmetricC r a Show Zone
  ) =>
  Show (Acceptance r a)
