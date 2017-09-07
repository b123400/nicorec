{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( login
    , extractWebSocketTokens
    , getM3U8Url
    , NicoException(..)
    ) where

import Prelude
import Control.Monad (mfilter, join)
import Control.Monad.Loops (untilJust)
import Control.Lens ((^.), (^?), (.~), (?~), ix, (<&>))
import Data.Functor (($>))
import Data.Function ((&))
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.Typeable (Typeable)
import Control.Monad.Catch (Exception, catchAll)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC8
import qualified Data.Text as T
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
import qualified Network.WebSockets as WS
import Network.URI (URI, parseURI, uriAuthority, uriRegName, uriPort, uriPath, uriQuery)
import Text.Regex.TDFA ((=~), AllTextSubmatches, getAllTextSubmatches)
import Data.List.Utils (replace)
import Data.String.Utils (maybeRead)
import Data.Aeson.Lens (key, _String)

import Error (liftErr)
import Cookie (renderCookieJar)

data NicoException = CannotLogin
                   | CannotExtractToken
                   | CannotParseWebsocketUri
  deriving (Show, Typeable)

instance Exception NicoException

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

websocketUri :: WebsocketTokens -> Maybe URI
websocketUri t = parse $ webSocketBaseUrl t
                      <> broadcastId t
                      <> "?audience_token="
                      <> audienceToken t
  where parse = parseURI . BC8.unpack

getM3U8Url :: WebsocketTokens -> CookieJar -> IO T.Text
getM3U8Url tokens jar =
  join $ WS.runClientWith
      <$> host
      <*> port
      <*> path
      <*> return WS.defaultConnectionOptions
      <*> return [("Cookies", B.toStrict cookieHeaderString)]
      <*> return action
  where uri = return (websocketUri tokens) >>= liftErr CannotParseWebsocketUri
        host :: IO String
        host = uriRegName <$> (uriAuthority <$> uri >>= liftErr CannotParseWebsocketUri)

        port :: IO Int
        port = (uri <&> uriAuthority
               >>= liftErr CannotParseWebsocketUri
               <&> uriPort
               <&> replace ":" ""
               <&> maybeRead
               >>= liftErr CannotParseWebsocketUri)
               `catchAll` (const $ return 80)

        path :: IO String
        path = (\u-> uriPath u <> uriQuery u) <$> uri

        cookieHeaderString = renderCookieJar jar

        action :: WS.Connection -> IO T.Text
        action c =
          putStrLn "Websocket connection opened"
          >> WS.sendTextData c ("{\"type\":\"watch\",\"body\":{\"params\":[\""
                             <> broadcastId tokens
                             <> "\",\"\",\"true\",\"hls\",\"\"],\"command\":\"getpermit\"}}")
          >> untilJust (WS.receiveData c
                        >>= \str -> BC8.putStrLn ("received json: " <> str)
                        $> lookForM3U8Url str)
          >>= (\url-> (putStrLn $ show url) $> url)

        lookForM3U8Url :: B.ByteString -> Maybe T.Text
        lookForM3U8Url string = string ^? key "body" . key "currentStream" . key "uri" . _String
