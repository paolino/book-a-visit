{-# language TypeFamilies #-}

module Inclusion where

-- | inclusion check class
class Include a where
  type Target a -- ^ the including type
  include :: a -> Target a -> Bool -- ^ inclusion check

