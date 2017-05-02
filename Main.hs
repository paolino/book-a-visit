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

module Main where

import Data.Bifunctor
import Control.Lens
import Data.Data.Lens
import Data.Data
import Data.Typeable
import Control.Lens.TH
import GHC.Base
import System.Random
import qualified Data.Map as M
import Status
import World
---------- example -------------------------------
type instance Bargain () = String
type instance Part u () = String
type instance Chat () = String
type instance Zone u () = (Float,Float,Maybe Float)
type instance Place u () = (Float,Float)
type instance Slot () = (Float,Float)
-- type instance Time () = (Float)
type instance Failure () = String
type instance Feedback () = String

instance SlotMatch () where
  data Time () = T Float
  matchLow (t0 , t1) (T t) = t >= t0
  matchHigh (t0 , t1) (T t) = t >= t1


deriving instance Show (World ())
main = do
  w <- step (NewI (randomIO, FromTaker "paolo veronelli" (New "visita ortopedica , 20€" (50 , 51) (24,23,Nothing)))) (mempty :: World ())
  print w
  let (i,k) = M.findMin $ w ^. proposalTaker
  print k
  Right w1 <- return $ step (OtherI $ FromGiver "vet dott Colla" (Appointment i (24,23))) w
  print w1

