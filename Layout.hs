{-# LANGUAGE RecursiveDo, QuasiQuotes,TemplateHaskell, ScopedTypeVariables, ConstraintKinds, FlexibleContexts#-}
module Layout where

import qualified Data.Map as M
import Reflex.Dom
import Control.Monad (void)
import Control.Monad
import GHCJS.DOM.Document (setTitle)
import Lib


mainWidgetWithAssets title description hs mcss w = mainWidget . void $ do

    askDocument >>= flip setTitle (Just title)
    el "style" $ text $ css
    maybe end (el "style" . text) mcss
    divClass "header" $ text $ description
    divClass "app" w

    mapM_ (\x -> sep >> showCode x) $ hs ++ [this]
    sep
    divClass "css" $ text css
