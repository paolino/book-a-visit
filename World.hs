{-# language TemplateHaskell #-}
{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language StandaloneDeriving #-}
{-# language ConstraintKinds #-}
{-# language GADTs #-}
{-# language UndecidableInstances #-}
{-# language TypeFamilies #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds #-}
{-# language FunctionalDependencies #-}
{-# language ScopedTypeVariables #-}
{-# language Rank2Types#-}
{-# language ViewPatterns #-}
{-# language TypeInType #-}

module World where

import Status
import Control.Lens.TH (makeLenses)
import Control.Lens hiding (dropping)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Kind
import Data.List
import Data.Ord
import Control.Arrow

newtype Idx (s::Phase) (u :: Presence Role)  = Idx Integer deriving (Eq, Ord, Show)
type MapW (s :: Phase) (u :: Presence Role) a = Map (Idx s u ) (Transaction s u a)

data World a = World
  {   _proposalGiver  ::  MapW ProposalT (Present Giver) a
  ,   _proposalTaker  ::  MapW ProposalT (Present Taker) a
  ,   _waiting        ::  MapW WaitingT Absent a
  ,   _serving        ::  MapW ServingT Absent a
  ,   _releasing      ::  MapW ReleasingT Absent a
  ,   _final          ::  MapW FinalT Absent a
  }


makeLenses ''World

instance Monoid (World a) where
  mempty = World mempty mempty mempty mempty mempty mempty 
  World s a v f q e  `mappend` World s' a' v' f' q' e'  =
    World   (s `mappend` s')
            (a `mappend` a')
            (v `mappend` v')
            (f `mappend` f')
            (q `mappend` q')
            (e `mappend` e')


data StepT = NewT | OtherT | TimeT

data Protocol r u a where
  New           :: Idx ProposalT (Present u) -> Bargain a 
                    -> Slot a -> Zone u a                    -> Protocol NewT u a
  Abort         :: Idx ProposalT (Present u)                 -> Protocol OtherT u a
  Appointment   :: Idx ProposalT (Present (Opponent u)) 
                        -> Place u a                          -> Protocol OtherT u a
  ChatWaiting   :: Idx WaitingT Absent -> Chat a              -> Protocol OtherT u a
  ChatServing   :: Idx ServingT Absent -> Chat a              -> Protocol OtherT u a
  ChatReleasing :: Idx ReleasingT Absent -> Chat a            -> Protocol OtherT u a
  Drop          :: Idx WaitingT Absent                        -> Protocol OtherT Giver a
  Fail          :: Idx ServingT Absent                        -> Protocol OtherT Giver a
  Success       :: Idx ReleasingT Absent -> Feedback a        -> Protocol OtherT Taker a


class Step (r :: StepT) u a where
  type Ctx r a :: Type -> Type
  step :: Protocol r u a -> Part u a -> World a -> Ctx r a (World a)


instance Step NewT Taker a where
  type Ctx NewT a = Identity -- ReaderT (Slot a) (Either NewErrors)
  step (New idx b s z) u w = Identity $ 
    proposalTaker . at idx .~ Just (Proposal (ProposalData b u z s)) $ w


instance Step NewT Giver a where
  type Ctx NewT a = Identity -- ReaderT (Slot a) (Either NewErrors)
  step (New idx b s z) u w = Identity $ 
    proposalGiver . at idx .~ Just (Proposal (ProposalData b u z s)) $ w

data OErrors = IndexNotFound | NotAllowed deriving (Show)

class Valid a b where
  valid :: a -> b -> Bool

instance (Valid (Zone Taker a) (Place Giver a),Eq (Part 'Giver a)) => Step OtherT Giver a where
  type Ctx OtherT a = Either OErrors
  step a@(Abort _) u w = abort proposalGiver EGiver a u w
  step a@(Appointment _ _ ) u w = accept proposalTaker ETaker a u w  
  step a@(ChatWaiting i c) _ w = chats (EGiver . RChat) i c waiting ChattingWaiting  w 
  step (Drop (Idx i)) _ w = move final waiting i w (const Nothing) Dropped
  step a@(ChatServing i c) _ w = chats (EGiver . RChat) i c serving ChattingServing w 
  step (Fail (Idx i)) _ w = move final serving i w (const Nothing) Failure
  step a@(ChatReleasing i c) _ w = chats (EGiver . RChat) i c releasing ChattingReleasing w 

instance (Valid (Zone Giver a) (Place Taker a),Eq (Part 'Taker a)) => Step OtherT Taker a where
  type Ctx OtherT a = Either OErrors
  step a@(Abort _) u w = abort proposalTaker ETaker a u w
  step a@(Appointment _ _ ) u w = accept proposalGiver EGiver a u w  
  step a@(ChatWaiting i c) _ w = chats (ETaker . RChat) i c waiting ChattingWaiting  w 
  step a@(ChatServing i c) _ w = chats (ETaker . RChat) i c serving ChattingServing w 
  step a@(ChatReleasing i c) _ w = chats (ETaker . RChat) i c releasing ChattingReleasing w 
  step (Success (Idx i) fb) _ w = move final releasing i w (const Nothing) (flip Successed fb)

chats :: (Chat a -> RoledChat a) 
        -> Idx s Absent
        -> Chat a
        -> Lens' (World a) (MapW s Absent a) 
        -> (Transaction s Absent a -> RoledChat a -> Transaction s Absent a)
        -> World a 
        -> Either OErrors (World a)

chats e (Idx i) c l f w = move l l i w (const Nothing {- should check ids -}) $
    \x ->  f x (e c)



accept :: (Reflexive u) => Lens' (World a) (MapW ProposalT (Present (Opponent u)) a) 
        -> (Transaction AcceptanceT (Present (Opponent u)) a -> PresenceRoled (Transaction AcceptanceT) a) 
        -> (Protocol OtherT u a) -> Part u a -> World a -> Either OErrors (World a)
accept l e a@(Appointment idx@(Idx i)  pl) u w = move waiting l i w  
    (const Nothing)
    (\x -> Waiting (e (Acceptance x $ AcceptanceData u pl)))


abort   :: Eq (Part u a) 
        => Lens' (World a) (MapW ProposalT (Present u) a) 
        -> (Transaction ProposalT (Present u) a -> PresenceRoled (Transaction ProposalT) a) 
        -> (Protocol OtherT u a) -> Part u a -> World a -> Either OErrors (World a)
abort l e (Abort idx@(Idx i)) u w = move final l i w 
    (\(Proposal (ProposalData b u' z s)) -> if u == u' then Nothing else Just NotAllowed)
    (Aborted . e)


move :: Lens' (World a) (MapW s u a)
     -> Lens' (World a) (MapW s' u' a)
     -> Integer
     -> World a
     -> (Transaction s' u' a -> Maybe OErrors)
     -> (Transaction s' u' a -> Transaction s u a)
     -> Either OErrors (World a)

move l1 l2 j w c f =     case w ^. l2 . at (Idx j) of
      Nothing -> Left IndexNotFound
      Just x -> case c x of
                  Nothing -> Right $ (l1 . at (Idx j) .~ Just (f x))  . (l2 . at (Idx j) .~ Nothing) $ w
                  Just e ->  Left e

getSlot :: (SummaryC p a) =>  Transaction s p a -> Slot a
getSlot x = view (proposal . slot) `through` summary x

-- | Presence Role disambiguity and Status hiding
data Box a  where
    TTaker :: Idx s (Present Taker) -> Transaction s (Present Taker)  a -> Box a
    TGiver :: Idx s (Present Giver) -> Transaction s (Present Giver)  a -> Box a
    TAbsent :: Idx s Absent -> Transaction s Absent a -> Box a

type State a = [Box a]

state :: Ord (Slot a) => World a -> State a
state w = map snd . sortBy (comparing fst) . concat  $ 
         [     map (getSlot . snd &&& uncurry TTaker) . M.assocs $ w ^. proposalTaker 
         ,     map (getSlot . snd &&& uncurry TGiver) . M.assocs $ w ^. proposalGiver
         ,     map (getSlot . snd &&& uncurry TAbsent) . M.assocs $ w ^. waiting
         ,     map (getSlot . snd &&& uncurry TAbsent)  . M.assocs $ w ^. serving
         ,     map (getSlot . snd &&& uncurry TAbsent) . M.assocs $ w ^. final
         ] 




{-


-}
