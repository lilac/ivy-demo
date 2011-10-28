{-# LANGUAGE OverloadedStrings #-}

{-|

This is where all the routes and handlers are defined for your site. The
'site' function combines everything together and is exported by this module.

-}

module Site
  ( site
  ) where

import           Data.Maybe
import qualified Data.Text.Encoding as T
import           Snap.Extension.Heist
import           Snap.Extension.Timer
import           Snap.Util.FileServe
import           Snap.Types
import           Text.Templating.Heist

import           Application
import           Web.Ivy.Routes
import           Web.Ivy.Types (handle)
import           Handlers.Echo (echo)
import           Handlers.Index (Index, index)
import qualified Data.ByteString.Char8 as BS
import Control.Isomorphism.Partial ((<$>))
import Text.Syntax  --((<|>), text, Syntax)

rIndex :: Syntax s => s Index
rIndex = index <$> text "/"

routes :: Syntax s => s (Route Application)
routes = routeIso <$> rIndex
    <|> routeIso <$> (echo <$> text "/echo/" *> string)
------------------------------------------------------------------------------
-- | The main entry point handler.
site :: Application ()
site = do
    req <- getRequest
    let path = rqURI req
    case parseUrl routes $ BS.unpack path of
        [Route r] -> handle r
        otherwise -> defApp

defApp = do
    req <- getRequest
    writeBS $ BS.pack $ show req
{-
site = route [ ("/",            index)
             , ("/echo/:stuff", echo)
             ]
       <|> serveDirectory "resources/static"
-}
