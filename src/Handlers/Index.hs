{-# LANGUAGE OverloadedStrings, TemplateHaskell, MultiParamTypeClasses, TypeSynonymInstances, DeriveDataTypeable #-}
module Handlers.Index where

import Snap.Types
import Application
import Snap.Extension.Heist
import Text.Templating.Heist
import Snap.Extension.Timer
import Control.Isomorphism.Partial.TH
import Web.Ivy.Types
import Data.Typeable

data Index = Index deriving (Show, Eq, Typeable)
$(defineIsomorphisms ''Index)

instance Handler Application Index where

------------------------------------------------------------------------------
-- | Renders the front page of the sample site.
    get _ = heistLocal (bindSplices indexSplices) $ render "index"
      where
        indexSplices =
            [ ("start-time",   startTimeSplice)
            , ("current-time", currentTimeSplice)
            ]

