{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( login
    , extractWebSocketTokens
    , getM3U8Url
    , processMasterM3U8
    ) where

import Prelude
import Control.Monad (mfilter, join)
import Control.Monad.Loops (untilJust)
import Control.Lens ((^.), (^?), (.~), (?~), (<&>), view)
import Data.Functor (($>))
import Data.Function ((&))
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.Traversable (traverse)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC8
import qualified Data.Text as T
import Network.HTTP.Client (CookieJar)
import Network.Wreq ( get
                    , post
                    , getWith
                    , defaults
                    , param
                    , cookies
                    , responseHeader
                    , responseHeaders
                    , responseBody
                    , responseCookieJar
                    , FormParam((:=)))
import qualified Network.WebSockets as WS
import Data.Aeson.Lens (key, _String)
import Safe (headMay)

import Lib.Error (liftErr, NicoException(..))
import Lib.Operators ((<*>>))
import Lib.Url (websocketUri, host, port, path, appendPath)
import Lib.Cookie (renderCookieJar)
import qualified Lib.Parser as P

login :: String -> String -> IO (Maybe CookieJar)
login username password = do
  putStrLn "Logging in"
  r <- post "https://secure.nicovideo.jp/secure/login" [ "mail" := username
                                                       , "password" := password ]
  let isLoginSuccess =
        r ^? responseHeader "x-niconico-authflag"
        & mfilter ((==) "1")
        & isJust

  putStrLn ("Login: " <> show isLoginSuccess)
  if isLoginSuccess
    then return $ r ^? responseCookieJar
    else return Nothing


fetchPlayerHTML :: CookieJar -> String -> IO B.ByteString
fetchPlayerHTML cookieJar liveID = do
  let opts = defaults & param "player_name" .~ ["leo"]
                      & cookies ?~ cookieJar
  r <- getWith opts $ "http://live2.nicovideo.jp/select_player/" ++ liveID
  return $ r ^. responseBody

extractWebSocketTokens :: CookieJar -> String -> IO (Maybe P.WebsocketTokens)
extractWebSocketTokens cookieJar liveID =
  fetchPlayerHTML cookieJar liveID <&> P.extractWebSocketTokens


getM3U8Url :: P.WebsocketTokens -> CookieJar -> IO T.Text
getM3U8Url tokens jar =
  (liftErr CannotParseWebsocketUri $ websocketUri tokens) >>= \uri->
  join $ WS.runClientWith
      <$> host uri
      <*> port uri
      <*>> (path uri)
      <*>> WS.defaultConnectionOptions
      <*>> [("Cookies", B.toStrict $ renderCookieJar jar)]
      <*>> action
  where action :: WS.Connection -> IO T.Text
        action c =
          putStrLn "Websocket connection opened"
          >> WS.sendTextData c ("{\"type\":\"watch\",\"body\":{\"params\":[\""
                             <> P.broadcastId tokens
                             <> "\",\"\",\"true\",\"hls\",\"\"],\"command\":\"getpermit\"}}")
          >> untilJust (WS.receiveData c
                        >>= \str -> BC8.putStrLn ("received json: " <> str)
                        $> lookForM3U8Url str)

        lookForM3U8Url :: B.ByteString -> Maybe T.Text
        lookForM3U8Url string = string ^? key "body" . key "currentStream" . key "uri" . _String


processMasterM3U8 :: String -> IO [String]
processMasterM3U8 url =
  get url
  <&> view responseBody
  <&> P.parsePlayListFromM3U8
  <&> headMay
  >>= liftErr NoPlayListFoundInMaster
  <&> BC8.unpack
  <&> appendPath url
  >>= liftErr InvalidPlayListPath
  >>= processPlayList

processPlayList :: String -> IO [String]
processPlayList url =
  get url
  <&> view responseBody
  <&> P.parsePlayListFromM3U8
  <&> (\l-> appendPath url <$> BC8.unpack <$> l >>= liftErr InvalidPlayListPath)
  >>= fetchAndShowHeaders

fetchAndShowHeaders :: [String] -> IO [String]
fetchAndShowHeaders urls = traverse showHeader urls
  where showHeader url = get url <&> view responseHeaders <&> show
