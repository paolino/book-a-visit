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
module Main where

import Lib -- (MS,ES,DS, Message, domMorph, EitherG(LeftG,RightG), rightG,leftG, Cable,sselect)
import Reflex.Dom hiding (Delete, Insert, Link)
import Data.Bifunctor
import Control.Lens hiding (dropping)
import Data.Data.Lens
import Data.Data
import Data.Typeable
import Control.Lens.TH
import GHC.Base
import System.Random
import qualified Data.Map as M
import Status
import World
import Data.Text
import Data.String.Here
import Data.String
import Control.Monad
import Data.Monoid
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

css = [here|
  div.region {
    background:yellow
    }
  |]

data Depth = Icon | Disclosed deriving Eq

deriving instance Show (World ())


-- data Context a =
showSummary (a,b) r = el "ul" $ do
      el "li" $ do
        text $ a <> ": "
        text $ pack $ (r ^. proposal . proponent)

      case r ^. acceptance of
          Nothing -> return ()
          Just a ->   do
            el "li" $ do
              text $ b <> ": "
              text $ pack $ (a ^. accepter)

      el "li" $ do
        text "when: "
        text $ pack $ show $ (r ^. proposal . slot)

      el "li" $ do
        text "where: "
        text $ pack $ case (r ^. acceptance) of
                        Nothing -> show (r ^. proposal . zone)
                        Just a -> show $ a ^. place

      el "li" $ do
        text "what: "
        text $ pack $ (r ^. proposal . bargain)

      let showChat t g (ETaker (RChat x)) = el "li" $ do
            text "client: "
            text $ pack $ x

          showChat t g (EGiver (RChat x)) = el "li" $ do
            text "business: "
            text $ pack $ x

      el "ul" $ do
          forM_ (r ^. chat) $ showChat (r ^. proposal . proponent) (fmap (view accepter) $ r ^. acceptance)
          t <-  textInput def
          return $ current (view textInput_value t) `tag` keypress Enter t

showRoledSummary :: MS m => Roled Summary () -> m (ES Text)
showRoledSummary s = case s of
            EGiver r -> showSummary ("business","client") r
            ETaker r -> showSummary ("client","business") r

slice :: forall m s pr  b. (SummaryC pr (), MS m) => Text -> Text -> DS (MapW s pr ()) -> m (ES b)
slice s s' l = divClass s $ do
  divClass "region" $ text s'
  let -- f :: MapW ProposalT pr a -> m (ES b)
      f l = do
        el "ul" $ forM_ (M.assocs l) $ \(Idx i,x) -> el "li" $ do
          rec   s <- holdDyn Icon e
                let f Icon =  (Disclosed <$) <$>  elClass "span" "icon" (button (pack $ Prelude.take 5 $ show $ i))
                    f Disclosed = elClass "span" "diclosed" $ do
                        e <- (Icon <$) <$>  button "close"
                        mesg <- showRoledSummary $ summary x
                        return e
                e <- domMorph f s
          return ()
        return never


  domMorph f l

fromIdx (Idx i) = Idx i
main = do
  w <- step (NewI (randomIO, FromTaker "paolo veronelli" (New "visita ortopedica , 20€" (50 , 51) (24,23,Nothing)))) (mempty :: World ())
  let (i,k) = M.findMin $ w ^. proposalTaker
  Right w1 <- return $ step (OtherI $ FromGiver "vet dott Colla" (Appointment i (24,23))) w
  Right w2 <- return $ step (OtherI $ FromTaker "paolo veronelli" (ChatWaiting (Chatting (fromIdx i) "grazie, ottimo"))) w1
  mainWidgetWithCss (fromString css) $ do
    b <- button "refresh"
    l  :: DS (World ()) <- holdDyn w2 (mempty <$ never) -- :: DS (World ())

    slice "proposalTaker" "Proposal from clients" $ view proposalTaker <$> l
    slice "proposalGiver" "Proposal from business" $ view proposalGiver <$> l
    slice "waiting" "Appointments" $ view waiting <$> l
    return ()
      {-
    slice "dropping" "Renounces" dropping l
    slice "serving" "Running" serving l
    slice "releasing" "Feedbackable" releasing l
    slice "final" "Done" proposalTaker l
-}

  return ()
