{-# LANGUAGE OverloadedStrings #-}
module Lib.Channel where

import Control.Lens ((?~), (&), (<&>), (^?), view, ix)
import Control.Monad ((>=>))
import Control.Monad.Catch (MonadThrow)
import Data.Monoid ((<>))
import Data.Maybe (listToMaybe, mapMaybe)
import Network.HTTP.Client (CookieJar)
import Network.Wreq (getWith, responseBody, cookies, defaults)
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (($.//), (&/), fromDocument, element, attributeIs, attribute)
import Text.Regex.TDFA ((=~), getAllTextMatches)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC8
import qualified Data.Text as T
import Lib.Error (liftErr, NicoException(NoLiveInCommunity))

url :: String -> String
url coId = "https://ch.nicovideo.jp/" <> coId <> "/live"

fetchChannelHTML :: CookieJar -> String -> IO B.ByteString
fetchChannelHTML cookieJar coId =
  let opts = defaults & cookies ?~ cookieJar
  in view responseBody <$> (getWith opts $ url coId)

getLiveID :: CookieJar -> String -> IO String
getLiveID coId cookieJar = findLiveID =<< fetchChannelHTML coId cookieJar

findLiveID :: MonadThrow m => B.ByteString -> m String
findLiveID html = liftErr NoLiveInCommunity $ go $ fromDocument $ parseLBS html
  where go cursor = (cursor $.// element "div" >=> attributeIs "id" "live_now_cnt"
                              &/ element "ul"
                              &/ element "li"
                              &/ element "div"
                              &/ element "p"
                              &/ element "a")
                 >>= attribute "href"
                 <&> T.unpack
                  &  mapMaybe liveIdInUrl
                  &  listToMaybe

        liveIdInUrl :: String -> Maybe String
        liveIdInUrl url =
           listToMaybe $ getAllTextMatches $ url =~ ("lv[0-9]+" :: String)

