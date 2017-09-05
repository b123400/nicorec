{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lib
    ( login
    , extractWebSocketTokens
    ) where

import Prelude
import Control.Monad (mfilter)
import Control.Lens ((^.), (^?), (.~), (?~), ix)
import Data.Function ((&))
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Client (CookieJar)
import Network.Wreq ( post
                    , getWith
                    , defaults
                    , param
                    , cookies
                    , responseHeader
                    , responseBody
                    , responseCookieJar
                    , FormParam((:=)))

import Text.Regex.TDFA ((=~), AllTextSubmatches, getAllTextSubmatches)

login :: String -> String -> IO (Maybe CookieJar)
login username password = do
  r <- post "https://secure.nicovideo.jp/secure/login" [ "mail" := username
                                                       , "password" := password ]
  let isLoginSuccess =
        r ^? responseHeader "x-niconico-authflag"
        & mfilter ((==) "1")
        & isJust

  if isLoginSuccess
    then return $ r ^? responseCookieJar
    else return Nothing


data WebsocketTokens = WebsocketTokens
  { broadcastId      :: B.ByteString
  , webSocketBaseUrl :: B.ByteString
  , audienceToken    :: B.ByteString
  } deriving (Show)

extractWebSocketTokens :: CookieJar -> String -> IO (Maybe WebsocketTokens)
extractWebSocketTokens cookieJar liveID = do
  let opts = defaults & param "player_name" .~ ["leo"]
                      & cookies ?~ cookieJar
  r <- getWith opts $ "http://live2.nicovideo.jp/select_player/" ++ liveID
  let bodyString = r ^.responseBody
  let tokens = WebsocketTokens
                <$> substringToken "broadcastId" bodyString
                <*> substringToken "webSocketBaseUrl" bodyString
                <*> substringToken "audienceToken" bodyString

  return tokens
  where substringToken :: B.ByteString -> B.ByteString -> Maybe B.ByteString
        substringToken name body =
          getAllTextSubmatches
          ((body =~ (name <> " *: *\"([^\"]+)\"")) :: AllTextSubmatches [] B.ByteString)
          ^? ix 1
