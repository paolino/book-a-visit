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
import Splash
import Simple

data Rolled = Rolled | UnRolled

rollable r s (c,w) = let
    f Rolled = divClass ("rolled " <> pack (show c)) $ do
        r <- divClass "rolling" $ (UnRolled <$) <$> floater (icon ["arrow-down","3x"] "unroll")
        divClass "slot" $ text . pack . show $ s
        return $ wire' LeftG r
    f UnRolled = divClass ("unrolled " <> pack (show c))$ do
        r <- divClass "rolling" $ (Rolled <$) <$> floater (icon ["arrow-up","3x"] "rollup")
        h <- divClass "transaction" $ w
        return $ merge [LeftG :=> r, RightG :=> h]
    in do
        rec     s <- holdDyn r (pick LeftG e)
                e <- domMorph f s
        return $ e
         
fromRight (Right x ) = x
fromSingleton = fst . fromJust . M.minView
-- listHoldWithKey :: forall t m k v a. (Ord k, DomBuilder t m, MonadHold t m) => Map k v -> Event t (Map k (Maybe v)) -> (k -> v -> m a) -> m (Dynamic t (Map k a))
-- worldDiff exw neww = (neww, stateDiff exw neww)
--
onlyInvolved :: (MS m,Eq (Part 'Taker a),Eq (Part Giver a)) => Roled Part a ->  Box a -> m (ES b) -> m (ES b)
onlyInvolved r b a = case throughBox (involved r . summary) b of
        True -> a
        _ -> return never

main = mainWidget $ do
    t <- divClass "info" $ runReaderT (icon ["question","2x"] "info") (constDyn False)
    d <- foldDyn (const not) False t
    flip runReaderT d $ do
        du <- fakeLogin


        rec dwaw  <- holdDyn (mempty,Nothing) $ 
                (\(i,new) -> (new, Just (i,findBox (snd i) new))) <$>
                (pick RightG cwe)
            let (dw,ddw) = splitDynPure dwaw
            icons <- foldDyn
                (\(i,s) m -> M.insert i s m)
                mempty
                (pick LeftG cwe)
            ddwi <- holdDyn mempty $ attachWith 
                (\im t -> case t of
                    Nothing -> mempty
                    Just (i,b) -> M.singleton  i $ flip (,) (maybe Rolled id $ M.lookup i im) <$> b )
                (current icons) 
                (updated ddw)
            let f (_ , (_,Nothing)) = divClass "nologin" splash >> return never
                f (im,(w,Just u)) = divClass "yeslogin" $ do
                    rec     wo  <- domMorph (proposalDriver u) dw
                            
                            wt  <- 
                                fmap  (fmap fromSingleton . switch . current . fmap mergeMap) . 
                                    listHoldWithKey (M.mapWithKey (\k b -> (b,maybe Rolled id $ M.lookup k im)) $ toState w) (updated ddwi) $ \i (b,r) -> onlyInvolved u b $ do
                                        e <- rollable r (fst i) (transaction u b)
                                        return $ merge [LeftG :=> ((,) i <$> pick LeftG e),
                                            RightG :=> (,) i <$> attachWith (\w f -> fromRight (f w)) (current dw) (pick RightG e)]

                    return $ leftmost [wire' RightG  wo,wt]
            cu <- holdDyn (mempty,(mempty,Nothing)) $ attach (current icons) $ attach (current dw) $ updated du
            cwe <- domMorph f $ cu 

{-        divClass "footer" . divClass "footer-fields" $ do 
        
            divClass "copyright" $ text "Paolo Veronelli ©"
            divClass "code-link" $ elAttr "a" [("href","https://github.com/paolino/book-a-visit")] $ text "code repository"
            divClass "code-api" $ elAttr "a" [("href","http://lambdasistemi.net/public/book-a-visit.jsexe/api")] $ text "api"
-}
        return ()

