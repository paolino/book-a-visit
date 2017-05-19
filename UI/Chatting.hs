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
{-# language PartialTypeSignatures #-}




module UI.Chatting where

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
import Constraints
import UI.ValueInput
import Control.Monad.Reader
import UI.Constraints
import UI.Lib
import HList
import Data.List

chatter
    :: forall a m u r s. ()
    => (MS m, MonadReader (DS r) m, In Bool r)
    => IconsU m Giver a
    => IconsU m Taker a
    => Showers a
    => Eqs Giver a
    => Eqs Taker a
    => Step 'OtherT u a
    => ShowPart a
    => Idx s 'Absent
   -> Transaction s Absent a
   -> Roled Part a
   -> Part u a
   ->  (Idx s 'Absent -> String -> Protocol OtherT u a)
   -> m (ES (World a -> Ctx OtherT a (World a)))
chatter i (summary -> s) me u q =  divClass "record waiting" $ do
    el "ul" $ do 
      el "li" $ do
        elAttr "span" [("class","field")] $ text "when"
        divClass "timeshow" $ text $ pack $ show $ through (view $ proposal . slot) s
      el "li" $ do
          elAttr "span" [("class","field")] $ text "where"
          divClass "placeshow" $  
            let g :: forall u . HasIcons m (Place (Opponent u) a) => Summary u a -> m (ES ())
                g = getIcon . fromJust . preview (acceptance . _Just . place)
            in case s of EGiver s -> g s
                         ETaker s -> g s
      el "li" $ do
        elAttr "span" [("class","field")] $ text "what"
        divClass "bargainshow" $ text $ pack $ through (view $ proposal . bargain) s
      el "li" $ do
        elAttr "span" [("class","field")] $ text "other"
        divClass "other" $ showPart  (head . delete me $ anyPart $ s)

      el "li" $ do
        elAttr "span" [("class","field")] $ text "chat"
        divClass "chats" $ el "ul" $ forM_ (reverse $ through (view chat) s) $ \c -> 
            el "li" $ showChat c
        divClass "chat-input" $ do
            g <- do 
                    t <- valueInput "you ...." Just
                    v <- fmap (ffilter isJust) . fmap (t `tagPromptlyDyn`) $ floater $ icon ["paper-plane-o","3x"] "send"
                    return $ fromJust <$> v

            return $ (\g -> step (q i g) u) <$> g


