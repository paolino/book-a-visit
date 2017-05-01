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

### _Proposal_ state

This fixes the first constraints for the appointment. The author can be _taker_ or _giver_.
A _proposal_ contains:

* the identification of the _proponent_ , 

* a fixed span of time, representing the time coordinate for the visit to come, 

* an extended definition of _zone_, representing the limit in space for the space coordinate of the visit and (*1)

* the _bargain_, representing the services to be exchanged during the visit.

    In version 0.1, only space as _zone_ is to be refined, but this will extend to both time and bargain if sensible.
    
(*1)* Only for Veterinarians doing home visits. Structures don't need it. 

## Interaction phase

Interaction phase starts with an appointment encoded in an _waiting_ state. An appointment can be extended with messages from both parties until the _visit_ time, this transactions are called _chatting_. _chatting_ doesn't change the nature of the status. _chatting_ itself can happen before and after the _visit_.
During the _visit_ phase the state can be set to _dropping_ from the _giver_ or reach its due time.
During the appointment time span  the only possible change of status is for the _giver_ with a _failed_ declaration sending it to a _failure_ final state.
After the appointment the chance for the _taker_ to close with a _success_ final state containing a _feedback_.
Even a _dropping_ state has to be closed to a _droppen_ final state with a _feedback_ from _taker_.
    
    Should we consider a different final state for droppeds?  Or even consider them negative?



### _Waiting_ state

The waiting is extending a proposal from the other _part_ and refining the proposed _zone_ to a defined _place_

A _waiting_ state contains:

* the originating proposal

* the identification of the _accepter_ and

* a location refining the _zone_ of the _proposal_ 

* a chat: an ongoing collection of messages from both parts

### _serving_ state

The _serving_ state is entered automatically during the appointment time. It is alternative to the _dropped_ state.
During _serving_ the state can only be transacted from the _giver_ to the _negative_ state when the _taker_ is not fulfilling the appointment.

A _serving_ state contains 

* the originating _chatting before_ state

* a chat: an ongoing collection of messages from both parts

### _Dropping_ state

A _dropped_ state is  created from the _giver_ only from the _chatting before_ state and is alternative to the _visit_ state. It represents a consensual decision of giving up the appointment. 

    Should it go back to proposal automatically ?
    
A _dropped_ state could be selected by _giver_ in all phases: before, during and after visit. The dropped state will be automatically obtained after 5 days. 

A _dropped_ state contains 

* the originating _chatting before_ state

### _Chatting after_ state

At the end of _visit_ the state moves automatically back to _chatting_ 

    Another time, why should chatting be stopped?

A _chatting after_ state contains

* the originating _visit_ state

* a chat: an ongoing collection of messages from both parts

## Final states

### _Negative_ state
This is the negative end of an appointment
A _negative_ state contains

* the originating _visit_ state

* a failure reason from the _giver_ (Good idea, we ask to _giver_ about) 

### _Positive_ state

This is the positive end of an appointment
A _positive_ state contains

* the original _chatting after_  or _dropped_ state 

* a feedback from the _taker_ (It's better sprinkle _taker_ to writing a review by the way actually offered. I don't uderstand if you want _taker_ write about itself experience in the chat or not.) 













