{-# language  OverloadedLists #-}
{-# language  DataKinds #-}
{-# language  NoMonomorphismRestriction #-}
{-# language  GADTs #-}
{-# language  FlexibleContexts #-}
import World
import Naive
import Locations
import Interaction
import Inclusion



utente = TakerR "paolino"
casa = Place (0,0) :: Place Taker Naive
ask = "visita ordinaria"
timex = Slot ([(0,20)])

w0 = naiveWorld
w1 = newOffer utente ask timex (Around $ Zone (0,0) 5) w0

w2 = w1 >>= \(w,i) -> dropAny i w
w3f = \(w,i) -> bookOffer i vet (AtAccepter (Place (1,1))) w
w3 = w1 >>= w3f

vet = GiverR "piero"
chat1 =  "capriole tutto il giorno"
chat2 = "non mi dire ...."
w4 = do
  x@(_,Ix i) <- w1
  (w3,()) <- w3f x
  takerChat (Ix i) chat1 w3



