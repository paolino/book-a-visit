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
import Data.Dependent.Map (DMap,DSum((:=>)), singleton)
import qualified Data.Dependent.Map as DMap
import Data.GADT.Compare (GCompare)
import Data.GADT.Compare.TH
import Lib -- (MS,ES,DS, Reason, domMorph, EitherG(LeftG,RightG), rightG,leftG, Cable,sselect)
import Reflex.Dom hiding (Delete, Insert, Link)
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
---------- example -------------------------------
type instance Bargain () = String
data instance Part Taker () = Client {fromClient :: String} deriving (Show,Eq)
data instance Part Giver () = Business {fromBusiness :: String} deriving (Show,Eq)
type instance Chat () = String
type instance Zone u () = (Float,Float,Maybe Float)
data instance Place u () = Place (Float,Float) deriving (Read, Show)
type instance Slot () = (Float,Float)
-- type instance Time () = (Float)
type instance Failure () = String
type instance Feedback () = String

instance SlotMatch () where

  data Time () = T Float
  matchLow (t0 , t1) (T t) = t >= t0
  matchHigh (t0 , t1) (T t) = t >= t1

css = [here|
  div.region {
    background:yellow
    }
  div.button.logger {
    color:red
  }
  div.small {
    font-size:80%;
    background:lightgrey;
    margin:2em
  }
  |]


deriving instance Show (World ())

open  :: MS m
      => Roled Part () -- who I am
      -> World ()
      -> m (ES (World ()))
open u w = el "ul" $ do
  bargain :: DS String <- el "li" $ do
          text "bargain: "
          fmap unpack <$> view textInput_value <$> textInput def

  zone :: DS String <- el "li" $ do
          text "zone: "
          fmap unpack <$> view textInput_value <$> textInput def

  time <- el "li" $ do
          text "time: "
          fmap unpack <$> view textInput_value <$> textInput def

  submit <- el "li" $ do
          button "submit"
  -- let tagger e = New <$> bargain <*> (read <$> time) <*> (read <$> zone)
  let (e :: ES (Reason NewT ()))  = case u of
          EGiver u -> fmap (FromGiver u) $ (New <$> bargain <*> (read <$> time) <*> (read <$> zone)) `tagPromptlyDyn` submit
          ETaker u -> fmap (FromTaker u) $ (New <$> bargain <*> (read <$> time) <*> (read <$> zone)) `tagPromptlyDyn` submit

  return $ pushAlways (\r -> liftIO $ step (NewI (randomIO, r)) w) e

data PrenoteA u a = PrenoteA (Idx ProposalT (Present (Opponent u))) (Part u a)

data Iconified  = Iconified | Disclosed

data TransitionOutput a where
  IconState :: TransitionOutput Iconified
  WorldChange :: TransitionOutput (World ())

deriveGEq ''TransitionOutput
deriveGCompare ''TransitionOutput


proposalWidget  :: (SummaryC ('Present u) (), MS m)
                => Transaction ProposalT (Present u) ()
                -> (Place (Opponent u) () -> Either Except (World ()))
                -> Iconified
                -> m (Cable TransitionOutput)

proposalWidget t _ Iconified  = do
  b <- divClass "icon" (button (pack $ show $ summary t))
  return $ wire (IconState :=> Disclosed <$ b)


proposalWidget t step Disclosed = do
  b <- divClass "abort" (button (pack "close"))
  let f Nothing = el "ul" $ do
          place :: DS String <- el "li" $ do
              text "place: "
              fmap unpack <$> view textInput_value <$> textInput def
          el "li" $ do
            b <- button "submit"
            return $ Just <$> ((read <$> place) `tagPromptlyDyn` b)
      f (Just e) = do
        divClass "error" $ text $ pack $ show e
        (Nothing <$) <$>  button "got it"
  rec   let   g p = step p
              w' = g <$> fmapMaybe id zm
              cm = leftmost [Just <$> lefting w',Nothing <$ ffilter isNothing zm]
        zm <- domMorph f m
        m <- holdDyn Nothing cm

  return $ merge [IconState :=> Iconified <$ b, WorldChange :=> righting w']


righting e = (\(Right x) -> x) <$> ffilter isRight e
lefting e = (\(Left x) -> x) <$> ffilter isLeft e

prenote :: (Reflexive u, SummaryC ('Present (Opponent u)) (), MS m)
        => Part u ()
        -> (Idx ProposalT (Present (Opponent u)) -> Place u () -> Either Except (World()))
        -> [(Idx ProposalT (Present (Opponent u)), Transaction ProposalT (Present (Opponent u)) ())]
        -> m (ES (World ()))

prenote u step xs = el "ul" $ (fmap leftmost) $ forM xs$ \(i,t) -> el "li" $ do

          rec   ws <- domMorph (proposalWidget t $ step i) s
                s <- holdDyn Iconified (pick IconState ws)

          return $ pick WorldChange ws

----------------------- fake login --------------------------------------

users = map (ETaker . Client) ["Paolo Veronelli","Patrizio Iezzi", "Mario Rossi"]
vets = map (EGiver . Business) ["Dottor Volatile", "Piero dei Pesci", "Marta dal Gatto"]

data Select a = Selected a | Selecting

fakeLogin :: MS m => m (DS (Maybe (Roled Part ())))
fakeLogin = do
  let f (Selected u) = (Selecting <$) <$> do
                        divClass "logger" $ button (pack $  maybe "login" show $ bimap  fromBusiness fromClient <$> u)
      f Selecting = (fmap Selected . leftmost) <$> do
        xs <- forM (users ++ vets) $ \u -> divClass "login" $
            (Just u <$) <$> button (pack $ show $ bimap  fromBusiness fromClient u)

        x <- (Nothing <$) <$>  divClass "logger" (button "logout")

        return $ x:xs

  rec change <-domMorph f login
      login <- holdDyn (Selected Nothing) change

  let selected (Selected x) = Just x
      selected _ = Nothing

  r <- holdDyn Nothing $ fmapMaybe selected change
  return $ r

----------------------------------------------------------------
main = do
  -- login
  mainWidgetWithCss (fromString css) $ do

    u <- fakeLogin

    let f (Nothing,w) = divClass "nologin" (text "please, login!") >> return never
        f (Just u,w) = divClass "logged" $ do
          wo <- divClass "propose" $ do
              el "h3" $ text "Your new proposal"
              open u w

          wp <- divClass "accept" $ case u of
              ETaker u -> case M.assocs $ w ^. proposalGiver of
                          [] -> return never
                          xs -> do
                              el "h3" $ text "Proposals you can take"
                              prenote u (\i p -> step (OtherI (FromTaker u (Appointment i p))) w) xs

              EGiver u ->case M.assocs $ w ^. proposalTaker of
                          [] -> return never
                          xs -> do
                              el "h3" $ text "Proposals you can take"
                              prenote u (\i p  -> step (OtherI (FromGiver u (Appointment i p))) w) xs

          divClass "abort" $ do
              el "h3" $ text "Proposals you can abort"

          return $ leftmost [wo,wp]

    rec   world :: DS (World ()) <- holdDyn mempty change
          change <- domMorph f $ (,) <$> u <*> world
    el "h3" $ text "This small world"
    divClass "small" $ dynText $ pack <$> show <$> world
    return ()
  return ()

