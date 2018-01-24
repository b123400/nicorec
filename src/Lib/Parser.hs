{-# LANGUAGE OverloadedStrings #-}

module Lib.Parser where

import Data.Monoid ((<>))
import Data.Maybe (listToMaybe, catMaybes, fromMaybe)
import Control.Lens ((^?), (<&>), (&), ix)
import Control.Monad ((>=>))
import Text.Regex.TDFA ((=~)
                       , AllTextSubmatches
                       , getAllTextSubmatches)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC8
import Data.String.Utils (maybeRead)
import Data.Aeson.Lens (key, _String)
import HTMLEntities.Decoder (htmlEncodedText)
import Text.XML.Cursor (($.//), fromDocument, element, attributeIs, attribute)
import Text.HTML.DOM (parseLBS)
import Debug.Trace (traceShow)

data WebsocketTokens = WebsocketTokens
  { broadcastId      :: B.ByteString
  , webSocketUrl     :: B.ByteString
  } deriving (Show)

extractWebSocketTokens :: B.ByteString -> Maybe WebsocketTokens
extractWebSocketTokens bodyString =
  WebsocketTokens
  <$> (encodeUtf8 <$> TL.fromStrict <$> broadcastId)
  <*> (encodeUtf8 <$> TL.fromStrict <$> webSocketUrl)

  where
    jsonString :: Maybe String
    jsonString = ((fromDocument $ parseLBS bodyString)
                   $.// (element "script")
                   >=> attributeIs "id" "embedded-data")
                 >>= attribute "data-props"
                 <&> htmlEncodedText
                 <&> toLazyText
                 <&> TL.unpack
                  &  listToMaybe
    broadcastId :: Maybe T.Text
    broadcastId = jsonString >>= \s->
                  s ^? key "program"
                     . key "broadcastId"
                     . _String
    webSocketUrl :: Maybe T.Text
    webSocketUrl = jsonString >>= \s->
                   s  ^? key "site"
                       . key "relive"
                       . key "webSocketUrl"
                       . _String

parsePlayListFromM3U8 :: BC8.ByteString -> [BC8.ByteString]
parsePlayListFromM3U8 = filter ((/=) '#' . BC8.head)
                      . filter ((/=) 0 . BC8.length)
                      . BC8.split '\n'

parseDurationFromM3U8 :: BC8.ByteString -> Int -- microseconds
parseDurationFromM3U8 = ((*) 1000000)           -- second -> microseconds
                      . foldl (const id) 1    -- default 1 second
                      . catMaybes
                      . map (maybeRead . BC8.unpack . last . BC8.split ':')
                      . filter (BC8.isPrefixOf "#EXT-X-TARGETDURATION:")
                      . BC8.split '\n'
