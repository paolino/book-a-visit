{-# language TypeFamilies #-} -- free implementations
{-# language DataKinds#-} -- phase  at type level
{-# language GADTs #-}  --  Location

{-# language FlexibleContexts #-} -- acceptance
--
-- {-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}

{-# language ConstraintKinds #-} -- synonyms of constraints


{-# language StandaloneDeriving #-} -- show and eq


{-# language PolyKinds #-} -- kinds introduction in type

module Locations where

import GHC.Base (Constraint, Type)



-- | Opponence relation for a kind (ko)
type family Opponent (a :: ka) :: ka

-- | Reflexive constraint for Opponent family
type Reflexive (a :: ka)  =
  Opponent (Opponent a) ~ a

type Symmetric (a::ka) (b :: ka) = (Opponent a ~ b, Opponent b ~ a)

-- | Symmetric constraint application for Opponent family
type SymmetricC (r :: kr) a (s :: Type -> Constraint) (u :: kr -> Type -> Type) =
  (s (u r a), s (u (Opponent r) a))

-- | inclusion check class
class Include a where
  type Target a -- ^ the including type
  include :: a -> Target a -> Bool -- ^ inclusion check

-- | values representing a place
data family Place (r :: kr) a

-- | values representing a zone
data family Zone (r :: kr) a

-- | Constraint for Zone and Place
type ZonePlace (r :: kr) a =  (Target (Zone r a) ~ Place r a, Include (Zone r a))

-- | type level phase distinction for locations
data Phase = Proposal | Acceptance

-- | a location parametrized by a role kind (kr)
data Location (b :: Phase) (r :: kr) a where
  -- | Proposal fixed place
  At :: Place r a -> Location Proposal r a
  -- | Acceptance of the proponent fixed place
  AtProponent :: Place (Opponent r) a ->  Location Acceptance r a
  -- | Acceptance of an accepter fixed place
  AtAccepter :: Place r a -> Location  Acceptance r a
  -- | Somewhere in a zone proposal
  Around :: Zone (Opponent r) a -> Location  Proposal r a
  -- | Somewhere in a zone or fixed place proposal
  AtAround :: Place r a -> Zone (Opponent r) a -> Location Proposal r a


-- | validity at term level of type level constrained Location change of phase
acceptance ::
    (   Eq (Place r a) -- just for proponent place
    ,   Reflexive r
    ,   ZonePlace (Opponent r) a -- just for accepter zone
    )
 => Location Proposal r a -> Location Acceptance (Opponent r) a -> Bool

acceptance (At p) (AtProponent q) = p == q
acceptance (Around z) (AtAccepter q) = z `include` q
acceptance (AtAround _ z) (AtAccepter q) = z `include` q
acceptance (AtAround p _) (AtProponent q) = p == q


deriving instance
  (   SymmetricC r a Show Place
  ,   SymmetricC r a Show Zone
  ) =>
  Show (Location b r a)
