
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Network.WebSockets as WS
import World
import Instance.Simple
import Constraints

main = print (mempty :: World S)
