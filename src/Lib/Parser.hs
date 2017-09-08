{-# LANGUAGE OverloadedStrings #-}

module Lib.Parser where

import Data.Monoid ((<>))
import Control.Lens ((^?), ix)
import Text.Regex.TDFA ((=~)
                       , AllTextSubmatches
                       , getAllTextSubmatches)
import qualified Data.ByteString.Lazy as B

data WebsocketTokens = WebsocketTokens
  { broadcastId      :: B.ByteString
  , webSocketBaseUrl :: B.ByteString
  , audienceToken    :: B.ByteString
  } deriving (Show)

extractWebSocketTokens :: B.ByteString -> Maybe WebsocketTokens
extractWebSocketTokens bodyString =
  WebsocketTokens
  <$> substringToken "broadcastId" bodyString
  <*> substringToken "webSocketBaseUrl" bodyString
  <*> substringToken "audienceToken" bodyString

  where substringToken :: B.ByteString -> B.ByteString -> Maybe B.ByteString
        substringToken name body =
          getAllTextSubmatches
          ((body =~ (name <> " *: *\"([^\"]+)\"")) :: AllTextSubmatches [] B.ByteString)
          ^? ix 1