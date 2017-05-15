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
    -> World a
    -> m (Event Spider (World a))

aborting i p u w = check (p ^. proponent == u) . divClass "record abort" $ do
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
    abortDriver (step (Abort i) u w) 


abortWidget 
  ::  forall r m a e . ()
  => (In Bool r, MonadReader (DS r) m, MS m) 
  => Show e
  => Either e (World a)
  -> Iconified
  -> m (Cable (EitherG Iconified (World a)))

abortWidget _ Iconified  = do
  b <- floater $  (icon ["close","3x"] "forget")
  return $ wire (LeftG :=> Disclosed <$ b)

abortWidget w Disclosed = do
  let f Nothing = el "ul" $ do
          divClass "modal" $ text "really want to abort the proposal?"
          (b,n) <- yesno (constDyn True)
          return $ wire' RightG $ leftmost [True <$ b ,False <$ n]
      f (Just e) = do
        divClass "error" $ text $ pack $ show e
        wire' LeftG <$>  icon ["check","3x"] "got it"
  rec   let
            w' = w <$ ffilter id (pick RightG zm)
            cm = leftmost [Just <$> lefting w',Nothing <$ pick LeftG zm]
        zm <- domMorph f m
        m <- holdDyn Nothing cm

  return $ merge [LeftG :=> Iconified <$ ffilter not (pick RightG zm), RightG :=> righting w']

abortDriver 
  :: ()
  => Show e
  => (In Bool r, MonadReader (DS r) m,  MS m) 
  => IconsU m Taker a
  => IconsU m Giver a
  =>  Either e (World a)
  -> m (ES (World a))

abortDriver step = do 
          rec   ws <- domMorph (abortWidget step) s
                s <- holdDyn Iconified (pick LeftG ws)

          return $ pick RightG ws

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
    -> World a
    -> Transaction ProposalT (Present u) a
    -> (Part u a -> Roled Part a)
    -> m (Event Spider (World a))


accepting i p u w x e = divClass "record accept" $ do
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
      acceptDriver (\ch -> step (Appointment i ch)  u w) x



acceptWidget 
  :: forall a r m e u. ()
  => (In Bool r, MonadReader (DS r) m, MS m) 
  => IconsU m u a
  => Show e
  => Valid (Zone u a) (Place (Opponent u) a)
  => (Place (Opponent u) a -> Either e (World a))
  -> Transaction ProposalT (Present u) a
  -> Iconified
  -> m (Cable (EitherG Iconified (World a)))

acceptWidget _ _ Iconified  = do
  b <- floater (icon ["handshake-o","3x"] "accept")
  return $ wire (LeftG :=> Disclosed <$ b)


acceptWidget  step (Proposal d) Disclosed = el "li" $ do
  elAttr "span" [("class","question")] $ text "choose a place"
  divClass "placepick" $ do
      let rs = filter (valid $ d ^. zone) [minBound .. maxBound] 
      let f :: Maybe e -> m (Cable (EitherG () (Maybe (Place (Opponent u) a))))
          f Nothing = do
                e <- fmap updated . divClass "radiochecks" $ radioChecks $ rs
                return $ wire (RightG :=> e)
          f (Just e) = do
            divClass "error" $ text $ pack $ show e
            (wire . (LeftG :=>)) <$> button "got it"
      
      rec   let
                eew = step <$> fmapMaybe id sel
                sel = pick RightG es
                cm = leftmost [Just <$> lefting eew,Nothing <$ pick LeftG es] --ES
            es <- domMorph f m
            m <- holdDyn Nothing cm

      selD <- holdDyn False (maybe False (const True) <$> sel)
      (y,n) <- yesno selD
      eewD <- holdDyn Nothing (Just  <$> righting eew)
      return $ merge [
        RightG :=> fmapMaybe id (eewD `tagPromptlyDyn` y)
        , LeftG :=> Iconified <$ n --
        ]

acceptDriver 
  :: ()
  => Show e
  => (In Bool r, MonadReader (DS r) m,  MS m) 
  => IconsU m u a
  => IconsU m (Opponent u) a
  => Valid (Zone u a) (Place (Opponent u) a)
  =>  (Place (Opponent u) a -> Either e (World a))
  -> Transaction ProposalT (Present u) a
  -> m (ES (World a))


acceptDriver step u = do
          rec   ws <- domMorph (acceptWidget step u) s
                s <- holdDyn Iconified (pick LeftG ws)
          return $ pick RightG ws

