{-
computing 2d geometry essentials
-}

module Geometry (encloser) where

import Control.Monad
import Data.List
import Control.Arrow
import Data.Ord

type P = (Float,Float)

emme :: P -> P -> Float
emme (x0,y0)(x1,y1) = (y1 - y0) / (x1 - x0)

type C = (P,Float)

inters :: Float -> C -> [P]
inters  m ((x,y),d) = [(x + dx, y + dy) | dx <- bs , dy <- bs] where
    bs = [dm,-dm]
    dm = d/sqrt(1 + m ^ 2)

apices :: C -> C -> ([P],[P])
apices c1@(p1,d1) c2@(p2,d2) = (f c1, f c2) where
  f = inters (emme p1 p2)

distance :: P -> P -> Float
distance (x0,y0) (x1,y1) = sqrt $ (x0 - x1) ^ 2 + (y0 - y1) ^ 2

diameters :: ([P],[P]) -> [(P,P)]
diameters (ps,qs) = liftM2 (,) ps qs

distances :: [(P,P)] -> [((P,P),Float)]
distances = sortBy (flip $ comparing snd) . map (id &&& uncurry distance)

middle :: P -> P -> P
middle (x0,y0) (x1,y1) = ((x0 + x1)/2,(y0 + y1)/ 2)

-- | find the minimum circle containing 2 other
encloser :: C -> C -> C
encloser x = (uncurry middle *** (/2)) . head . distances . diameters . apices x
