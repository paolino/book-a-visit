# Semantic of the service, the appointment flow

## Introduction

Booking a visit is about deciding time and place for a professional to give his/her services, and easing interaction during the necessary time to fulfill the service.

In the documentation we will refer to the professional as the _giver_ and to the client as the _taker_ , to both together as _parts_ and to the services as a _visit_.
Deciding a span of time and place is the generally referred as taking an _appointment_. 

*Book-a-visit* takes care of the _appointment_ phase and the _interaction_ phase, which is the span of time between the appointment and an event in time after the appointment which close the _interaction_ and makes it a _final_.

The encoding and intricacies involved in the _appointment_ and _interaction_ phases are the challenge we strive to resolve with the protocol defined in the rest of this document.

## Depicted flow

![flow](https://raw.githubusercontent.com/paolino/book-a-visit/master/book-a-services.jpg)

## Appointment phase

The appointment phase is a symmetric two step transition involving both parts.
One part is called _proponent_ and it's the author of the first transition, creating the _proposal_. The other part is called _accepter_ and it's the author of the second step, the _acceptance_.
Parts can be any of _giver_ and _taker_, but they must be _opponent_ to each other in the two transitions.

### _Proposal_ state

This state fixes the first constraints for the appointment. The author can be _taker_ or _giver_.
A _proposal_ contains:

* the identification of the _proponent_ , 

* a fixed span of time, representing the time coordinate for the visit to come, 

* an extended definition of _zone_, representing the limit for the space coordinate of the visit and 

* the _bargain_, representing the service to be exchanged during the visit.

    In version 0.1, only space as _zone_ is to be refined, but this will extend to both time and bargain if sensible.
    

## Interaction phase

Interaction phase starts with an appointment encoded in an _waiting_ state. An appointment can be extended with messages from both parties until the end.
This transactions are called _chatting_. _chatting_ doesn't change the nature of the state. 
During the _serving_ phase (aka the actual _visit_) the state can be transitioned to _dropping_ by the _giver_ or reach its due time.
During the _serving_  the only possible change of state is for the _giver_ with a _failed_ declaration sending it to a _failure_ final state.
After the _serving_ , the state is automatically transitioned to _releasing_ when the chance is for the _taker_ to transition to a _success_ final state containing a _feedback_.
Also _dropping_ states should to be closed to a _dropped_ final state with a _feedback_ from _taker_. A _timeout_ will automatically close _dropping_ and _releasing_ states if the _taker_ is not closing them.
    

### _Waiting_ state

The _waiting_ state is a _proposal_ extended with the other _part_ and a refinement of the proposed _zone_, we call a _place_

A _waiting_ state contains:

* the originating proposal

* the identification of the _accepter_ and

* a _place_ refining the _zone_ of the _proposal_ 

* a chat: an ongoing collection of messages from both parts

### _Serving_ state

The _serving_ state is entered automatically at the appointment time. It is alternative to the _dropped_ state.
During _serving_ the state can only be transacted from the _giver_ to the _negative_ state when the _taker_ is not fulfilling the appointment.

A _serving_ state contains 

* the originating _chatting before_ state

* a chat: an ongoing collection of messages from both parts

### _Dropping_ state

A _dropping_ state is  created from the _giver_ only from the _waiting_ state and is alternative to the _serving_ state. 
It represents a consensual decision of giving up the appointment. 

A _dropping_ state contains 

* the originating _waiting_ state

### _Releasing_ state

This state is necessary for the _taker_ to give a final feedback on the interaction

A _releasing_ state contains

* the originating _visit_ state

* a chat: an ongoing collection of messages from both parts

## Final states

### _Failure_ state

This is the negative end of an appointment
A _failure_ state contains

* the originating _visit_ state

* a failure reason from the _giver_ 

### _Success_ state

This is the positive end of an appointment
A _success_ state contains

* the originating _releasing_ state 

* a feedback from the _taker_ 


### _Dropped_ state

This is the half experience where interaction has happenend but no _serving_ has
A _success_ state contains

* the originating _dropping_ state 

* a feedback from the _taker_  











