{-# LANGUAGE OverloadedStrings, TemplateHaskell, MultiParamTypeClasses, TypeSynonymInstances, DeriveDataTypeable #-}
module Handlers.Echo where

import Snap.Types
import Application
import Snap.Extension.Heist
import Text.Templating.Heist
import Snap.Extension.Timer
import Data.Text (pack)
import Data.Maybe
import Control.Applicative
import Control.Isomorphism.Partial.TH
import Web.Ivy.Types
import Data.Typeable

data Echo = Echo String deriving (Show, Eq, Typeable)
$(defineIsomorphisms ''Echo)

instance Handler Application Echo where
------------------------------------------------------------------------------
-- | Renders the echo page.
    get (Echo message)  =
        heistLocal (bindString "message" (pack message)) $ render "echo"
      where
        decodedParam p = fromMaybe "" <$> getParam p
