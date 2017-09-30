{-# LANGUAGE OverloadedStrings #-}

module Lib.Parser where

import Data.Monoid ((<>))
import Data.Maybe (catMaybes)
import Control.Lens ((^?), ix)
import Text.Regex.TDFA ((=~)
                       , AllTextSubmatches
                       , getAllTextSubmatches)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC8
import Data.String.Utils (maybeRead)

data WebsocketTokens = WebsocketTokens
  { broadcastId      :: B.ByteString
  , webSocketUrl     :: B.ByteString
  } deriving (Show)

extractWebSocketTokens :: B.ByteString -> Maybe WebsocketTokens
extractWebSocketTokens bodyString =
  WebsocketTokens
  <$> substringToken "broadcastId" bodyString
  <*> substringToken "webSocketUrl" bodyString

  where substringToken :: B.ByteString -> B.ByteString -> Maybe B.ByteString
        substringToken name body =
          getAllTextSubmatches
          ((body =~ (name <> " *: *\"([^\"]+)\"")) :: AllTextSubmatches [] B.ByteString)
          ^? ix 1

parsePlayListFromM3U8 :: BC8.ByteString -> [BC8.ByteString]
parsePlayListFromM3U8 =
  filter ((/=) '#' . BC8.head) . filter ((/=) 0 . BC8.length) . BC8.split '\n'

parseDurationFromM3U8 :: BC8.ByteString -> Int -- microseconds
parseDurationFromM3U8 =
  ((*) 1000000)           -- second -> microseconds
  . foldl (const id) 1    -- default 1 second
  . catMaybes
  . map (maybeRead . BC8.unpack . last . BC8.split ':')
  . filter (BC8.isPrefixOf "#EXT-X-TARGETDURATION:")
  . BC8.split '\n'
