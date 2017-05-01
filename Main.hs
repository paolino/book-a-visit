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

import Status

---------- example -------------------------------
type instance Bargain () = String
type instance Part u () = String
type instance Chat () = String
type instance Zone u () = (Float,Float,Float)
type instance Place u () = (Float,Float)
type instance Slot () = (Float,Float)
type instance Time () = (Float)
type instance Failure () = String
type instance Feedback () = String

p0 = Proposal (ProposalData "ciao" "paolo" (0,0,1) (0,10)) :: Transaction ProposalT Giver ()
p1 = Waiting . EGiver $ Acceptance p0  (AcceptanceData "piero" (0,0))
p2 = ChattingWaiting p1 $ EGiver $ RChat "grazie piero, vuoi consigli ?"

main = do
  let h x = putStrLn $ "........... " ++ x ++ " ............."
  h "p0"
  print p0
  print $ summary p0
  h "p1"
  print p1
  print $ summary p1
  h "p2"
  print  p2
  print $ summary p2
  h "access chat p2"
  print $ summary p2  ^? _EGiver . chat
