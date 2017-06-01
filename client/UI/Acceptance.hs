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
{-# language InstanceSigs #-}




module UI.Acceptance where

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
import Status
import World
import Data.Text (Text,pack,unpack)
import Data.String.Here
import Data.String
import Control.Monad
import Data.Maybe
import Data.Monoid
import Control.Monad.Trans
import Data.Either
import Text.Read (readMaybe)
import UI.Constraints
import UI.ValueInput
import Control.Monad.Reader
import UI.Constraints
import Constraints
import HList

aborting 
    :: ()
    => (MS m, MonadReader (DS r) m, In Bool r)
    => IconsU m Taker a
    => IconsU m Giver a
    => Eqs Taker a
    => Eqs Giver a
    => Eqs u a
    => IconsU m u a
    => Show (Slot a)
    => Bargain a ~ String
    => Step 'OtherT u a
    => Idx 'ProposalT ('Present u)
    -> ProposalData u a
    -> Part u a
    -> m (ES (World a -> Ctx OtherT a (World a)))

aborting i p u = check (p ^. proponent == u) . divClass "record abort" $ do
    el "ul" $ do 
      el "li" $ do
        elAttr "span" [("class","field")] $ text "when"
        divClass "timeshow" $ text $ pack $ show $ p ^. slot
      el "li" $ do
          elAttr "span" [("class","field")] $ text "where"
          divClass "placeshow" $ getIcon $ p ^. zone
      el "li" $ do
        elAttr "span" [("class","field")] $ text "what"
        divClass "bargainshow" $ text $ pack $ p ^. bargain
    abortDriver (step (Abort i) u) 


abortWidget 
  ::  forall  m r  . (MS m)
  => (MS m, MonadReader (DS r) m, In Bool r)
  => Iconified
  -> m (Cable (EitherG Iconified ()))

abortWidget Iconified  = do
  b <- floater $  (icon ["close","3x"] "forget")
  return $ wire (LeftG :=> Disclosed <$ b)

abortWidget Disclosed = do
    p <- el "ul" $ do
          divClass "modal" $ text "really want to abort the proposal?"
          (b,n) <- yesno (constDyn True)
          return $ leftmost [True <$ b ,False <$ n]
    return $ merge [RightG :=> () <$ ffilter id p , LeftG :=> Iconified <$ ffilter not p]

abortDriver 
  :: ()
    => (In Bool r, MonadReader (DS r) m,  MS m) 
--  => IconsU m Taker a
--  => IconsU m Giver a
  => a
  ->  m (ES a)

abortDriver x = do 
          rec   ws <- domMorph abortWidget s
                s <- holdDyn Iconified (pick LeftG ws)

          return $ x <$ pick RightG ws

----------------------------------------------------------------
---------------- Appointment ----------------------------------
----------------------------------------------------------------
----------------------------------------------------------------
----------------------------------------------------------------

accepting
    :: ()
    => (MS m, MonadReader (DS r) m, In Bool r)
    => Reflexive u
    => Eqs u a
    => IconsU m (Opponent u) a
    => IconsU m u a
    => Show (Slot a)
    => Bargain a ~ String
    => Step 'OtherT (Opponent u) a
    => ShowPart a
    => Valid (Zone u a) (Place (Opponent u) a)
    => Idx 'ProposalT ('Present u)
    -> ProposalData u a
    -> Part (Opponent u) a
    -> Transaction ProposalT (Present u) a
    -> (Part u a -> Roled Part a)
    -> m (ES (World a -> Ctx OtherT a (World a)))


accepting i p u x e = divClass "record accept" $ do
    el "ul" $ do 
      el "li" $ do
        elAttr "span" [("class","field")] $ text "when"
        divClass "timeshow" $ text $ pack $ show $ p ^. slot
      el "li" $ do
          elAttr "span" [("class","field")] $ text "where"
          divClass "placeshow" $ getIcon $ p ^. zone
      el "li" $ do
        elAttr "span" [("class","field")] $ text "what"
        divClass "bargainshow" $ text $ pack $ p ^. bargain
      el "li" $ do
        elAttr "span" [("class","field")] $ text "proponent"
        divClass "opponent" $ showPart $ e (p ^. proponent)
      acceptDriver (\ch -> step (Appointment i ch)  u) x



acceptWidget 
  :: forall a r m z u. ()
  => (In Bool r, MonadReader (DS r) m, MS m) 
  => IconsU m u a
  => Valid (Zone u a) (Place (Opponent u) a)
  => (Place (Opponent u) a -> z)
  -> Transaction ProposalT (Present u) a
  -> Iconified
  -> m (Cable (EitherG Iconified z))

acceptWidget _ _ Iconified  = do
  b <- floater (icon ["handshake-o","3x"] "accept")
  return $ wire (LeftG :=> Disclosed <$ b)


acceptWidget  step (Proposal d) Disclosed = el "li" $ do
  elAttr "span" [("class","question")] $ text "choose a place"
  divClass "placepick" $ do
      let rs = filter (valid $ d ^. zone) [minBound .. maxBound] 
      -- pd :: m (DS (Maybe (Place (Opponent u) a)))
      pd <- divClass "radiochecks" $ radioChecks $ rs

      (y,n) <- yesno $ maybe False (const True) <$> pd
      
      return $ merge [
        RightG :=> fmapMaybe id ((fmap step <$>  pd) `tagPromptlyDyn` y)
        , LeftG :=> Iconified <$ n --
        ]

acceptDriver 
  :: ()
  => (In Bool r, MonadReader (DS r) m,  MS m) 
  => IconsU m u a
  => IconsU m (Opponent u) a
  => Valid (Zone u a) (Place (Opponent u) a)
  =>  (Place (Opponent u) a -> z)
  -> Transaction ProposalT (Present u) a
  -> m (ES z)


acceptDriver step u = do
          rec   ws <- domMorph (acceptWidget step u) s
                s <- holdDyn Iconified (pick LeftG ws)
          return $ pick RightG ws

