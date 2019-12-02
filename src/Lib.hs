{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( login
    , extractWebSocketTokens
    , record
    ) where

import Prelude
import Control.Monad (mfilter, join, forM_)
import Control.Monad.Loops (untilJust, whileJust_)
import Control.Lens ((^.), (^?), (.~), (?~), (<&>), view)
import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Concurrent.MVar (MVar, putMVar, takeMVar, tryTakeMVar, newEmptyMVar)
import Control.Concurrent.STM.TMQueue (newTMQueue, writeTMQueue)
import Control.Monad.Catch (MonadCatch, MonadThrow, catchAll, catch, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.STM (atomically)
import Data.Functor (($>))
import Data.Function ((&))
import Data.List (any)
import Data.Maybe (isJust, catMaybes, listToMaybe, fromMaybe)
import Data.String.Utils (maybeRead)
import Data.Monoid ((<>))
import Data.Traversable (traverse)
import qualified Data.ByteString as BS -- ByteString Strict
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC8
import qualified Data.Text as T
import Network.HTTP.Client (CookieJar, HttpException, cookie_name)
import Network.HTTP.Client.Internal (expose)
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
import Data.Aeson.Lens (key, nth, _String, _Number)
import System.FilePath.Posix (takeFileName, (</>), (<.>))

import Conduit ( ConduitT
               , MonadIO
               , ResourceT
               , MonadResource
               , Void
               , (=$=)
               , await
               , liftIO
               , yieldMany
               , yield
               , runResourceT
               , runConduit
               , (.|) )
import Data.Conduit.Async ((=$=&), runCConduit)
import Data.Conduit.TQueue (sourceTMQueue)
import Network.HTTP.Simple (parseRequest, httpSource, getResponseBody)
import Data.Conduit.Binary (sinkFile)
import Data.Time.Clock.POSIX (getPOSIXTime)

import Lib.Error (liftErr, NicoException(..))
import Lib.Operators ((<*>>))
import Lib.Url (websocketUri, host, port, path, appendPath)
import Lib.Cookie (renderCookieJar)
import Lib.FileSystem (prepareDirectory)
import Lib.Utility (repeatDedup, retry)
import Lib.Conduit (dedup, fork, collect)
import qualified Lib.Parser as P

import qualified Control.Exception as E

import Wuss

login :: String -> String -> IO (Maybe CookieJar)
login username password = do
  putStrLn "Logging in"
  r <- post "https://secure.nicovideo.jp/secure/login" [ "mail" := username
                                                       , "password" := password ]
  let isLoginSuccess = fromMaybe False $ any ((==) "user_session" . cookie_name)
                                      <$> expose
                                      <$> (r ^? responseCookieJar)

  putStrLn ("Login: " <> show isLoginSuccess)
  if isLoginSuccess
    then return $ r ^? responseCookieJar
    else return Nothing


fetchPlayerHTML :: CookieJar -> String -> IO B.ByteString
fetchPlayerHTML cookieJar liveID = do
  let opts = defaults & param "player_name" .~ ["leo"]
                      & cookies ?~ cookieJar
  r <- getWith opts $ "http://live2.nicovideo.jp/watch/" ++ liveID
  return $ r ^. responseBody

extractWebSocketTokens :: CookieJar -> String -> IO (Maybe P.WebsocketTokens)
extractWebSocketTokens cookieJar liveID =
  fetchPlayerHTML cookieJar liveID <&> P.extractWebSocketTokens

record :: P.WebsocketTokens -> CookieJar -> String -> IO ()
record tokens jar outputFolderPath = do
  uri <- liftErr CannotParseWebsocketUri $ websocketUri tokens
  prepareDirectory outputFolderPath
  join $ (runSecureClientWith
          <$> host uri
          <*> pure 443
          <*>> path uri
          <*>> WS.defaultConnectionOptions
          <*>> [("Cookies", B.toStrict $ renderCookieJar jar)
               ,("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/68.0.3440.106 Safari/537.36")
               ]
          <*>> processWebsocket)
  where
    processWebsocket :: WS.Connection -> IO ()
    processWebsocket c = do
      putStrLn "Websocket connection opened"
      WS.sendTextData c ("{\"type\": \"watch\", \"body\": {\"command\": \"playerversion\", \"params\": [\"leo\"]}}" :: BC8.ByteString)
      WS.sendTextData c ("{\"type\":\"watch\",\"body\":{\"command\":\"getpermit\",\"requirement\":{\"broadcastId\":\""
                       <> P.broadcastId tokens
                       <> "\",\"route\":\"\",\"stream\":{\"protocol\":\"hls\",\"requireNewStream\":true,\"priorStreamQuality\":\"abr\",\"isLowLatency\":true, \"isChasePlay\": false},\"room\":{\"isCommentable\":true,\"protocol\":\"webSocket\"}}}}")

      runResourceT $ do
        tsSource <- fetchPlaylist $ wsSource c
                                 .| handlePingpong c
                                 .| handleWatchingInterval c (P.broadcastId tokens)
                                 .| filterCurrentStreamM3u8
                                 .| fetchMasterPlaylist
        timeStamp <- liftIO getPOSIXTime

        runCConduit $ tsSource
                   =$=& (dedup 100)
                   =$=& fetch
                   =$=& fork
                   =$=& collect
                   =$=& (sink $ show (round timeStamp) <.> "ts")
      putStrLn "Websocket closed"

    wsSource :: MonadIO m => MonadThrow m => WS.Connection -> ConduitT () BC8.ByteString m ()
    wsSource c = do
      mstr <- liftIO $ catch (Just <$> WS.receiveData c)
                             (\e -> case e of
                                        WS.CloseRequest _ _ -> pure Nothing
                                        WS.ConnectionClosed -> pure Nothing
                                        _ -> throwM e)
      case mstr of
        Nothing -> pure ()
        Just str | (str ^? key "body" . key "command" . _String) == Just "disconnect" -> pure ()
                 | otherwise -> yield str >> wsSource c

    handlePingpong :: MonadIO m => WS.Connection -> ConduitT BC8.ByteString BC8.ByteString m ()
    handlePingpong c = whileJust_ await $ \message->
      case (message ^? key "type" . _String) of
        Just "ping" -> do
          liftIO $ putStrLn "Sending pong..."
          liftIO $ WS.sendTextData c ("{\"type\":\"pong\",\"body\":{}}" :: BC8.ByteString)
        _ -> yield message

    handleWatchingInterval :: MonadIO m => WS.Connection -> B.ByteString -> ConduitT BC8.ByteString BC8.ByteString m ()
    handleWatchingInterval c broadcastId =
      let beforeFork = do
            mmessage <- await
            case mmessage of
              Nothing -> pure ()
              Just message ->
                case (lookForWatchingInterval message) of
                  Nothing -> yield message >> beforeFork
                  Just interval -> do
                    liftIO $ forkIO (responseWatchingInterval c broadcastId interval)
                    afterFork
          afterFork = do
            message <- await
            case message of
              Nothing -> pure ()
              Just m -> yield m >> afterFork
      in beforeFork

    responseWatchingInterval :: WS.Connection -> B.ByteString -> Int -> IO ()
    responseWatchingInterval c broadcastId interval = do
      putStrLn "Sending watching command"
      WS.sendTextData c ("{\"type\":\"watch\",\"body\":{\"command\":\"watching\",\"params\":[\""
                      <> broadcastId
                      <> "\",\"-1\",\"0\"]}}")
      threadDelay interval
      responseWatchingInterval c broadcastId interval

    -- Find stream urls and only yield the urls
    filterCurrentStreamM3u8 :: Monad m => ConduitT BC8.ByteString String m ()
    filterCurrentStreamM3u8 = do
      whileJust_ await $ \message ->
        case lookForM3U8Url message of
          Just uri -> yield $ T.unpack uri
          Nothing -> pure ()

    fetchMasterPlaylist :: MonadIO m => MonadThrow m => ConduitT String String m ()
    fetchMasterPlaylist = whileJust_ await $ \url->
          (liftIO $ get url)
      <&> view responseBody
      <&> P.parsePlayListFromM3U8
      <&> listToMaybe
      >>= liftErr NoPlayListFoundInMaster
      <&> BC8.unpack
      <&> appendPath url
      >>= liftErr InvalidPlayListPath
      >>= yield

    fetchPlaylist :: MonadIO m => ConduitT () String m () -> m (ConduitT () String m ())
    fetchPlaylist con = do
      queue <- liftIO $ atomically newTMQueue
      currentThread <- liftIO $ newEmptyMVar

      let loopFetch url = do
            body <- view responseBody <$> (retry 20 $ get url)
            let playList = catMaybes $ appendPath url <$> BC8.unpack
                                                      <$> P.parsePlayListFromM3U8 body
                duration = P.parseDurationFromM3U8 body

            atomically $ forM_ playList (writeTMQueue queue)

            -- Try to reduce missing frame by making interval lower
            threadDelay $ round $ (fromIntegral duration) * 0.8
            loopFetch url

      runConduit $ con .| (do
        murl <- await
        case murl of
          Nothing -> pure ()
          Just url -> liftIO $ do
            tid <- tryTakeMVar currentThread
            maybe (pure ()) killThread tid
            newTid <- forkIO $ loopFetch url
            putMVar currentThread newTid)

      pure $ sourceTMQueue queue

    fetch :: MonadIO m => ConduitT String (ConduitT () BS.ByteString (ResourceT IO) ()) m ()
    fetch = whileJust_ await $ \url-> do
              req <- liftIO $ parseRequest url
              yield $ httpSource req getResponseBody

    sink :: MonadResource m => String -> ConduitT BS.ByteString Void m ()
    sink filename = sinkFile $ outputFolderPath </> filename

    lookForM3U8Url :: B.ByteString -> Maybe T.Text
    lookForM3U8Url string = string ^? key "body"
                                    . key "currentStream"
                                    . key "uri"
                                    . _String

    -- Niconico requires client side to report "I am watching" for every interval
    -- We can get the interval from websocket,
    -- The report will be handled in `responseWatchingInterval`
    lookForWatchingInterval :: B.ByteString -> Maybe Int
    lookForWatchingInterval string =
      ((*) 1000000) <$> (maybeRead =<< findInterval string)
      where findInterval string  = (string ^? key "body" . key "command")
                                   &   mfilter ((==) "watchinginterval")
                                   >>  string ^? key "body"
                                               . key "params"
                                               . nth 0
                                               . _String
                                   <&> T.unpack
