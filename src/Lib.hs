{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lib
    ( login
    ) where

import Prelude
import Control.Exception (throwIO)
import Control.Monad (mfilter)
import Control.Lens ((^?))
import Data.Functor (($>))
import Data.Function ((&))
import Data.Maybe (isJust)
import qualified Data.ByteString.Char8 as B
import Network.HTTP.Client (CookieJar)
import Network.Wreq ( post
                    , responseHeader
                    , responseHeaders
                    , responseCookieJar
                    , FormParam((:=)))

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
    else return $ Nothing
