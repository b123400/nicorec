{-# LANGUAGE OverloadedStrings #-}
module Cookie
    ( renderCookieJar
    ) where

import Data.Functor (fmap)
import Data.Binary.Builder (toLazyByteString)
import Network.HTTP.Client (CookieJar)
import Web.Cookie (renderCookies)
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Client (destroyCookieJar, cookie_name, cookie_value)

renderCookieJar :: CookieJar -> B.ByteString
renderCookieJar = toLazyByteString
                . renderCookies
                . fmap (\c-> (cookie_name c, cookie_value c))
                . destroyCookieJar
