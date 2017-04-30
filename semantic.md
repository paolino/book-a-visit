# Semantic of the service, the appointment flow

## Introduction

Booking a visit is about deciding time and place for a professional to give his/her services, and organizing events during the necessary time to fulfill the service.

In the documentation we will refer to the professional as the _giver_ and to the client as the _taker_ , to both together as _parts_ and to the services as a _visit_.
Deciding a span of time and place is the generally referred as taking an _appointment_. 

*Book-a-visit* takes care of the _appointment_ phase and the _interaction_ phase, which is the span of time between the appointment and an event in time after the appointment which close the _interaction_ and makes it a _final_.

The encoding and intricacies involved in the _appointment_ and _interaction_ phases are the challenge we strive to resolve with the protocol defined in the rest of this document.

## Depicted flow

![flow](book-a-services.svg)

## Appointment phase

The appointment phase is a symmetric two transaction involving both parts.
One part is called _proponent_ and it's the author of the first transaction, creating the _proposal_. The other part is called _accepter_ and it's the author of the second state, the _acceptance_.
Parts can be any of _client_ and _taker_, but they must be _opponent_ to each other in the two transactions.

### Proposal state

A _proposal_ contains:

* the identification of the _proponent_ , 

* a fixed span of time, representing the time coordinate for the visit to come, 

* an extended definition of _zone_, representing the limit in space for the space coordinate of the visit and 

* the _bargain_, representing the services to be exchanged during the visit.

    In version 0.1, only space as _zone_ is to be refined, but this will extend to both time and bargain if sensible.


## Interaction phase

Interaction phase starts with an appointment encoded in an acceptance status. An appointment can be extended with messages from both parties until the _serving_ time, this transactions are called _chatting_. _chatting_ doesn't change the nature of the status. _chatting_ itself can happen before and after the _serving_.
During the _serving_ phase the state can be set to _dropped_ from the _giver_ or reach its due time and go on to _chatting after_ state.
During the appointment time span chatting and dropping are disabled and the only possible change of status is from the _giver_ with a _failure_ declaration. _Failure_ is a possible final state.
After the appointment chatting is re-enabled together with the chance for the _taker_ to close with an end state containing a _feedback_.
_Dropped_ state has to be closed with a _feedback_ from _taker_.

### Chatting state

The chatting state is extending a proposal with the other _part_ and refining the _zone_ to a _place_

A _chatting before_ state contains:

* the originating proposal

* the identification of the _accepter_ and

* a location refining the _zone_ of the _proposal_ 

* a chat: an ongoing collection of messages from both parts

### _Serving_ state

The _serving_ state is entered automatically during the appointment time. It is alternative to the _dropped_ state.
During _serving_ the state can only be transacted from the _giver_ to the _failure_ state when the _taker_ is not fulfilling the appointment.

A _servicing_ state contains 

* the originating _chatting before_ state

### _Dropped_ state

A _dropped_ state is  created from the _giver_ only from the _chatting before_ state and is alternative to the _serving_ state. It represents a consensual decision of giving up the appointment. 

    Should it go back to proposal automatically ?

A _dropped_ state contains 

* the originating _chatting before_ state

### _Chatting after_ state

At the end of _serving_ the state moves automatically back to _chatting_ 

A _chatting after_ state contains

* the originating _serving_ state

* a chat: an ongoing collection of messages from both parts

## Final states

### _Failure_ state
This is the negative end of an appointment
A _failure_ state contains

* the original _serving_ state

* a failure reason from the _giver_

### _Feedback_ state

This is the positive end of an appointment
A _feedback_ state contains

* the original _chatting after_  or _dropped_ state 

* a feedback from the _taker_













