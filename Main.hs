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


        rec dwaw  <- foldDyn 
                (\(i,new) (old,_) -> (new, M.singleton i $ findBox (snd i) new))
                (mempty,mempty) 
                cwe
            let (dw,ddw) = splitDynPure dwaw

            let f (_,Nothing) = divClass "nologin" (divClass "splash" $ text "Book a Visit") >> return never
                f (w,Just u) = divClass "yeslogin" $ do
                    rec     wo :: ES (WKey S,World S) <- domMorph (proposalDriver u) dw
                            
                            wt :: ES (WKey S, World S -> Ctx OtherT S (World S)) <- 
                                fmap  (fmap fromSingleton . switch . current . fmap mergeMap) . 
                                    listHoldWithKey (toState w) (updated ddw) $ \i b -> onlyInvolved u b $ do
                                        -- let i Iconified = 
                                        -- id <- holdDyn Iconified ie
                                        ((,) i <$>) <$> transaction u b

                    return $ leftmost [wo,attachWith (\w (i,f) -> (i,fromRight $ f w)) (current dw) wt]
            cu <- holdDyn (mempty,Nothing) $ attach (current dw) $ updated du
            cwe :: ES (WKey S, World S) <- domMorph f $ cu 

        performEvent_ $ (\e -> liftIO (putStrLn $ "number of updates:" ++ show (M.size e))) <$> updated ddw
        divClass "footer" . divClass "footer-fields" $ do 
        
            divClass "copyright" $ text "Paolo Veronelli ©"
            divClass "code-link" $ elAttr "a" [("href","https://github.com/paolino/book-a-visit")] $ text "code repository"
            divClass "code-api" $ elAttr "a" [("href","http://lambdasistemi.net/public/book-a-visit.jsexe/api")] $ text "api"

        return ()

