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
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, putMVar, takeMVar, newEmptyMVar)
import Control.Monad.Catch (catch)
import Data.Functor (($>))
import Data.Function ((&))
import Data.Maybe (isJust, catMaybes, listToMaybe)
import Data.String.Utils (maybeRead)
import Data.Monoid ((<>))
import Data.Traversable (traverse)
import qualified Data.ByteString as BS -- ByteString Strict
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC8
import qualified Data.Text as T
import Network.HTTP.Client (CookieJar, HttpException)
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

import Conduit ( Source
               , Producer
               , Conduit
               , Sink
               , MonadIO
               , ResourceT
               , MonadResource
               , (=$=)
               , ($$)
               , await
               , liftIO
               , yieldMany
               , yield
               , runResourceT
               , runConduit )
import Data.Conduit.Async ((=$=&), runCConduit)
import Data.Conduit.TQueue (sinkTMQueue)
import Control.Concurrent.STM.TMQueue (TMQueue, newTMQueue, readTMQueue)
import Control.Monad.STM (atomically)
import Network.HTTP.Simple (parseRequest, httpSource, getResponseBody)
import Data.Conduit.Binary (sinkFile)
import Data.Time.Clock.POSIX (getPOSIXTime)

import Lib.Error (liftErr, NicoException(..))
import Lib.Operators ((<*>>))
import Lib.Url (websocketUri, host, port, path, appendPath)
import Lib.Cookie (renderCookieJar)
import Lib.FileSystem (prepareDirectory)
import Lib.Utility (repeatDedup)
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
getM3U8Url tokens jar = do
  uri <- liftErr CannotParseWebsocketUri $ websocketUri tokens
  result <- newEmptyMVar
  forkIO $ (join $ WS.runClientWith
            <$> host uri
            <*> port uri
            <*>> path uri
            <*>> WS.defaultConnectionOptions
            <*>> [("Cookies", B.toStrict $ renderCookieJar jar)]
            <*>> action result)
  takeMVar result

  where action :: MVar T.Text -> WS.Connection -> IO ()
        action result c =
          putStrLn "Websocket connection opened"
          >> WS.sendTextData c ("{\"type\":\"watch\",\"body\":{\"params\":[\""
                             <> P.broadcastId tokens
                             <> "\",\"\",\"true\",\"hls\",\"\"],\"command\":\"getpermit\"}}")
          >> untilJust (WS.receiveData c
                        >>= \str -> BC8.putStrLn ("received json: " <> str)
                        $> lookForM3U8Url str)
          >>= putMVar result
          >> untilJust (WS.receiveData c
                        >>= \str -> BC8.putStrLn ("received json: " <> str)
                        $> lookForWatchingInterval str)
          >>= forkIO . (watchingInterval c (P.broadcastId tokens))
          >> keepReading c
          >> putStrLn "Websocket closed"

        watchingInterval c broadcastId interval =
          putStrLn "Sending watching command"
          >> WS.sendTextData c ("{\"type\":\"watch\",\"body\":{\"command\":\"watching\",\"params\":[\""
                                <> broadcastId
                                <> "\",\"-1\",\"0\"]}}")
          >> threadDelay interval
          >> watchingInterval c broadcastId interval

        pong c = putStrLn "Sending pong..."
                 >> WS.sendTextData c ("{\"type\":\"pong\",\"body\":{}}" :: BC8.ByteString)

        keepReading c = WS.receiveData c
                        >>= \str -> BC8.putStrLn ("received data: " <> str)
                        >> (if isJust $ mfilter ((==) "ping") (str ^? key "type" . _String)
                           then pong c
                           else return ())
                        >> keepReading c

        lookForM3U8Url :: B.ByteString -> Maybe T.Text
        lookForM3U8Url string = string ^? key "body"
                                        . key "currentStream"
                                        . key "uri"
                                        . _String

        -- Niconico requires client side to report "I am watching" for every interval
        -- We can get the interval from websocket,
        -- The report will be handled in `watchingInterval`
        lookForWatchingInterval :: B.ByteString -> Maybe Int
        lookForWatchingInterval string =
          ((*) 1000000) <$> (maybeRead =<< findInterval string)
          where findInterval string  = (string ^? key "body" . key "command")
                                       & mfilter ((==) "watchinginterval")
                                       >> string ^? key "body" . key "params" . nth 0 . _String
                                       <&> T.unpack

processMasterM3U8 :: String -> String -> IO ()
processMasterM3U8 base url =  prepareDirectory base
                              >> get url
                              <&> view responseBody
                              <&> P.parsePlayListFromM3U8
                              <&> listToMaybe
                              >>= liftErr NoPlayListFoundInMaster
                              <&> BC8.unpack
                              <&> appendPath url
                              >>= liftErr InvalidPlayListPath
                              >>= processPlayList base


processPlayList :: String -> String -> IO ()
processPlayList base url = do
  timeStamp <- round <$> getPOSIXTime
  runResourceT $ runCConduit $ source
                            =$=& (dedup 100)
                            =$=& fetch
                            =$=& fork
                            =$=& collect
                            =$=& (sink $ show timeStamp <.> "ts")

  where source :: MonadIO m => Conduit () m String
        source = do
          liftIO $ putStrLn $ "fetching " <> url
          body <- liftIO $ view responseBody <$> get url
          let playList = catMaybes $ appendPath url <$> BC8.unpack
                                                    <$> P.parsePlayListFromM3U8 body
          let duration = P.parseDurationFromM3U8 body
          yieldMany playList
          liftIO $ threadDelay (round $ (fromIntegral duration) * 0.8) -- Try to reduce missing frame by making interval lower
          -- need to figure out when to end
          source

        dedup :: Eq a => Monad m => Int -> Conduit a m a
        dedup limit = dedup' limit []
        dedup' limit existing = do
          val <- await
          case val of
            Nothing -> return ()
            Just v  -> do
              let isDup = elem val existing
              if not isDup then yield v else return ()
              let newExisting = if isDup
                                then existing
                                else drop (length existing + 1 - limit) (existing <> [val])
              dedup' limit newExisting

        fetch :: MonadIO m => Conduit String m (Conduit () (ResourceT IO) BS.ByteString)
        fetch = do
          url <- await
          case url of
            Nothing -> return ()
            Just u -> do
              req <- liftIO $ parseRequest u
              yield $ httpSource req getResponseBody
              fetch

        fork :: MonadIO m => Conduit (Conduit () (ResourceT IO) a) m (TMQueue a)
        fork = do
          stream <- await
          case stream of
            Nothing -> return ()
            Just source -> do
              q <- liftIO $ atomically newTMQueue
              liftIO $ forkIO $ runResourceT $ runConduit $ source =$= (sinkTMQueue q True)
              yield q
              fork

        collect :: MonadIO m => Conduit (TMQueue a) m a
        collect = do
          queue <- await
          case queue of
            Nothing -> return ()
            Just q -> do
              drainAll q
              collect
          where drainAll q = do
                             val <- liftIO $ atomically $ readTMQueue q
                             case val of
                               Nothing -> return ()
                               Just v  -> yield v >> drainAll q

        sink :: MonadResource m => String -> Sink BS.ByteString m ()
        sink filename = sinkFile $ base </> filename

outputFilename :: String -> String -> String
outputFilename base url =
  base </> (takeWhile ((/=) '?') $ takeFileName url)
