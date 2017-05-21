{-# language MultiParamTypeClasses, FlexibleInstances, TypeOperators, OverlappingInstances  #-}

module HList where

class In l ls where
    -- | estrae l'elemento di tipo l
    see :: ls -> l        
    -- | sostituisce l'elemento di tipo l
    set :: l -> ls -> ls    

instance In l (l,ls) where 
    see (l,_) = l 
    set l (_,ls) = (l,ls) 

instance In l ls => In l (l',ls) where 
    
    see (_,ls) = see ls
    set l (l',ls) = (l',set l ls)

instance {-# OVERLAPS #-} In l l where
    see l = l
    set l l' = l

-- | modifica l'elemento di tipo l 
seeset     :: In l ls 
    => (l -> l)     -- ^ modificatore 
    -> ls         -- ^ struttura iniziale
    -> ls        -- ^ struttura finale
seeset f x =  set  (f $ see x) x


infixr 8 .<

-- | compositore di struttura, x <. y == (x,y)
(.<) :: l -> ls -> (l,ls)
(.<) = (,) 

infixr 8 :*:

-- | compositore di tipi 
type a :*: b = (a,b)
  
