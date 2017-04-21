{-# language NoMonomorphismRestriction #-}
module Spans where

import Data.List (sortBy,foldr1, unfoldr)
import Data.Ord (comparing)

data Span a = Span {
  start :: a,
  duration :: a
  } deriving Show

type Spans a = [Span a]

sortSpans :: (Num a , Ord a) => Spans a -> Spans a
sortSpans =  sortBy (comparing start)

collapse x@(Span s1 d1) y@(Span s2 d2)
  | s1 + d1 >= s2 = Left $ Span s1 (max d1 $ s2 + d2 - s1)
  | otherwise = Right (x,y)

collapsing :: (t -> t -> Either t (t, t)) -> [t] -> [t]
collapsing _ [] = []
collapsing f (x:xs) = g x xs where
  g x [] = [x] -- end
  g x (y:ys) = case f x y of
    Right (x,y) -> x : g y ys
    Left y -> g y ys

reduce = collapsing collapse . sortSpans
{-
finiteRight f = foldr1 (\[x] (y:ys) -> either return (\(x,y) -> [x,y]) (f x y) ++ ys) . map return

headHunter :: (a -> a -> Either a (a, a)) -> [a] -> [a]
headHunter collapse = concat . unfoldr f where
  f (x:y:xs) = case collapse x y of
                 Right (x,y) -> Just ([x],y:xs)
                 Left y -> Just ([],y:xs)
  f [x] = Just ([x],[])
  f [] = Nothing
-}
