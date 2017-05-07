{-# language ScopedTypeVariables #-}
module Instance.Date where

import Text.Printf


data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Show,Bounded,Enum)
data Delta = Delta ATime ATime
data ATime = ATime Float
instance Show ATime where
  show (ATime x) = let
    h :: Int = floor x
    m :: Int = floor $ (x - fromIntegral h)*60
    in printf "%02d:%02d" h m

instance Show Delta where
  show (Delta s e) = show s ++ "-" ++ show e

data Date = Date Day Delta

instance Show Date where
  show (Date x y) = show x ++ " at " ++ show y

