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




-- https://youtu.be/btyhpyJTyXg?list=RDG8yEe55gq2c
--
module Main where
import Data.List
import Data.Ord
import Data.Dependent.Map (DMap,DSum((:=>)), singleton)
import qualified Data.Dependent.Map as DMap
import Data.GADT.Compare (GCompare)
import Data.GADT.Compare.TH
import UI.Lib -- (MS,ES,DS, Reason, domMorph, EitherG(LeftG,RightG), rightG,leftG, Cable,sselect)
import Reflex.Dom hiding (Delete, Insert, Link, Abort)
import Data.Bifunctor
import Control.Lens hiding (dropping)
import Data.Data.Lens
import Data.Data
import Data.Typeable
import Control.Lens.TH
import System.Random
import qualified Data.Map as M
import Data.Text (Text,pack,unpack)
import Data.String.Here
import Data.String
import Control.Monad
import Data.Maybe
import Data.Monoid
import Control.Monad.Trans
import Control.Monad.Reader
import Data.Either
import UI.Proposal
import UI.FakeLogin
import UI.Constraints
import Instance.Simple

import UI.Transactions

import Status
import World
import Constraints

data Rolled = Rolled | UnRolled

rollable b w = let
    f Rolled = divClass "rolled" $ do
        r <- divClass "rolling" $ (UnRolled <$) <$> floater (icon ["arrow-down","3x"] "unroll")
        divClass "slot" $ text . pack . show $ throughBox getSlot b
        return $ wire' LeftG r
    f UnRolled = divClass "unrolled" $ do
        r <- divClass "rolling" $ (Rolled <$) <$> floater (icon ["arrow-up","3x"] "rollup")
        h <- divClass "transaction" $ w
        return $ merge [LeftG :=> r, RightG :=> h]
    in do
        rec     s <- holdDyn Rolled (pick LeftG e)
                e <- domMorph f s
        return $ pick RightG e
         



main = mainWidget $ do
    t <- divClass "info" $ runReaderT (icon ["question","2x"] "info") (constDyn False)
    d <- foldDyn (const not) False t
    flip runReaderT d $ do
      u <- fakeLogin

      let f (Nothing,w) = divClass "nologin" (divClass "splash" $ text "Book a Visit") >> return never
          f (Just u,w) = divClass "yeslogin" $ do
                            wo <- proposalDriver u w
                            wt <- leftmost <$> mapM (\x -> rollable x $ transaction u x w) (state w)
                            return $ leftmost [wo,wt]

      rec   w :: DS (World S) <- holdDyn mempty dw
            dw <- domMorph f $ (,) <$> u <*> w

      divClass "footer" . divClass "footer-fields" $ do 
        
        divClass "copyright" $ text "Paolo Veronelli ©"
        divClass "code-link" $ elAttr "a" [("href","https://github.com/paolino/book-a-visit")] $ text "code repository"
        divClass "code-api" $ elAttr "a" [("href","http://lambdasistemi.net/public/book-a-visit.jsexe/api")] $ text "api"

      return ()

