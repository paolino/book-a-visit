# Semantic of book-a-visit transactions

## Introduction

Booking a visit is about deciding time and place for a professional to give his/her services, and organizing events during the necessary time to fullfill them.

In this document we will refer to the professional as the _giver_ and to the client as the _taker_ , to both together as _parts_ and to the services as a _visit_.
Deciding a span of time and place is the generally referred as taking an _appointment_. 

*Book-a-visit* takes care of the _appointment_ phase and the _interaction_ phase, which is the span of time between the appointment and an event in time after the appointment which close the _interaction_ and makes it a _visit_.

The encoding and intricacies involved in the _appointment_ and _interaction_ phases are the challenge we strive to resolve with the protocol in the rest of this document.

## Appointment phase

The appointment phase is a two state interaction involving both parts.
One part is called _proponent_ and it's the author of the first state, the _proposal_. The other is called _accepter_ and it's the author of the second state, the _acceptance_.
Parts can be any of _client_ and _taker_, but they must be _opponent_ to each other in the two states.

### Proposal state

A _proposal_ contains:

* the identification of the _proponent_ , 

* a fixed span of time, representing the time coordinate for the visit to come, 

* an extended definition of _zone_, representing the limit in space for the space coordinate of the visit and 

* the _bargain_, representing the services to be exchanged during the visit.


    In version 0.1, only space as _zone_ is to be refined, but this will extend to both time and bargain if sensible.


### Acceptance state

An _acceptance_ contains:

* the proposal intended to be accepted,

* the identification of the _accepter_ and

* a location refining the _zone_ of the _proposal_ 

The acceptance is thus extending a proposal with the other _part_ and refining the _zone_ to a _place_ 
The acceptance is the base state to enter the _interaction_ phase






