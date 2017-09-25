{-# LANGUAGE OverloadedStrings #-}

module Lib.Url where

import Control.Monad.Catch (MonadThrow, MonadCatch, catchAll)
import Control.Lens ((&), (<&>))
import Data.Monoid ((<>))
import Data.List.Utils (replace)
import Data.String.Utils (maybeRead)
import qualified Data.ByteString.Lazy.Char8 as BC8
import Network.URI ( URI
                   , parseURI
                   , uriAuthority
                   , uriRegName
                   , uriPort
                   , uriPath
                   , uriQuery
                   , parseURIReference
                   , relativeTo )

import Lib.Error (liftErr, NicoException(..))
import Lib.Parser (WebsocketTokens, webSocketUrl, broadcastId)

websocketUri :: WebsocketTokens -> Maybe URI
websocketUri t = parse $ webSocketUrl t
  where parse = parseURI . BC8.unpack


host :: MonadThrow m => URI -> m String
host uri = uriAuthority uri
           <&> uriRegName
            & liftErr CannotParseWebsocketUri

port :: MonadThrow m => MonadCatch m => URI -> m Int
port uri = (uriAuthority uri
           <&> uriPort
           <&> replace ":" ""
           >>= maybeRead
            & liftErr CannotParseWebsocketUri)
           `catchAll` (const $ return 80)

path :: URI -> String
path uri = uriPath uri <> uriQuery uri

appendPath :: String -> String -> Maybe String
appendPath base path =
  show <$> (relativeTo <$> parseURIReference path <*> parseURI base)
