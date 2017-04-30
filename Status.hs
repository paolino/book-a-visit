{-# language DataKinds #-}
{-# language TypeFamilies #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}

module States where

import Data.Bifunctor

type Box0 f = Either (f Giver)  (f Taker)
type Box f a = Either (f Giver a)  (f Taker a)

data Role = Giver | Taker | Any
data Phase = BootT | ProposalT | WaitingT | DroppedT | ServingT | ReleasingT | DroppingT | FinalT | AcceptanceT

type family Part (u :: Role) a

type family Opponent u where
  Opponent Giver = Taker
  Opponent Taker = Giver


type family Zone (u :: Role) a
type family Place (u :: Role) a
type family Slot a
type family Time a
type family Bargain a
type family Chat a
type family Feedback a
type family Failure a

data RoledChat a = RoledChat (Chat a) (Either (Part Giver a)  (Part Taker a))

data State s (u :: Role) a where

  Proposal :: Bargain a -> Part u a -> Zone u a -> Slot a -> State ProposalT u a

  -- | dropping a proposal
  Aborted :: Box (State ProposalT) a -> State FinalT Any a

  -- | Accepting a proposal (ephimeral)
  Acceptance :: State ProposalT u a -> Part (Opponent u) a -> Place (Opponent u) a -> State AcceptanceT u a

  -- | chatting before initial status
  Waiting :: Box (State AcceptanceT) a -> State WaitingT Any a

  -- | chatting before recursive status
  ChattingWaiting :: State WaitingT Any a -> RoledChat a -> State WaitingT Any a

  -- | Consensual end
  Dropping :: State WaitingT Any a -> State DroppingT Any a


  Serving :: State WaitingT Any a -> State ServingT Any a

  -- | chatting during service time
  ChattingServing :: State ServingT Any a -> RoledChat a -> State ServingT Any a

  Releasing :: State ServingT Any a -> State ReleasingT Any a

  ChattingReleasing :: State ReleasingT Any a -> RoledChat a -> State ReleasingT Any a

  Successed :: State ReleasingT Any a -> Feedback a -> State FinalT Any a

  Dropped :: State DroppingT Any a -> Feedback a -> State FinalT Any a

  Failure :: State ServingT Any a -> Failure a -> State FinalT Any a

data family Index (s :: Phase) (u :: Role)

type Map f (s :: Phase) (u :: Role) a = f (Index s u) (State s u a)

data World f a = World
  {   _proposalGiver  ::  Map f ProposalT Giver a
  ,   _proposalTaker  ::  Map f ProposalT Taker a
  ,   _waiting        ::  Map f WaitingT Any a
  ,   _serving        ::  Map f ServingT Any a
  ,   _releasing      ::  Map f ReleasingT Any a
  ,   _final          ::  Map f FinalT Any a
  }



data Chatting s a = Chatting (Index s Any) (Chat a)

data Protocol u a where
  New :: Bargain a -> Slot a -> Zone u a -> Protocol u a
  Givup :: Index ProposalT u -> Protocol u a
  Appointment :: Index ProposalT u -> Place (Opponent u) a -> Protocol u a
  ChatWaiting :: Chatting WaitingT a -> Protocol u a
  ChatServing :: Chatting ServingT a -> Protocol u a
  ChatReleasing :: Chatting ReleasingT a -> Protocol u a
  StartDrop :: Index WaitingT Any -> Protocol Giver a
  Fail :: Index ServingT Any -> Failure a -> Protocol Giver a
  Success :: Index ReleasingT Any -> Feedback a -> Protocol Taker a
  EndDrop :: Index DroppingT Any -> Feedback a -> Protocol Giver a

data Event a = FromGiver (Part Giver a) (Protocol Giver a) | FromTaker (Part Taker a) (Protocol Taker a) | Tick (Time a)

class Matching a b where
  match :: a -> b -> Bool


data Problem  = NotMatching | WrongAuthor | NotFound

type Step f a = Event a -> World f a -> Either Problem (World f a)


acceptance :: State s Any a -> Box (State AcceptanceT) a
acceptance (Waiting p) = p
acceptance (ChattingWaiting p _) = acceptance p
acceptance (Serving p) = acceptance p
acceptance (ChattingServing p _) = acceptance p
acceptance (Releasing p) = acceptance p
acceptance (ChattingReleasing p _) = acceptance p
acceptance (Dropping p) = acceptance p
acceptance (Successed p _) = acceptance p
acceptance (Failure p _) = acceptance p
acceptance (Dropped p _) = acceptance p


slot :: State s u a -> Slot a
slot (Proposal _ _ _ s) = s
slot (Acceptance p _ _) = slot p
slot =


