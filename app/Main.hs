module Main where

import Control.Lens ((<&>))
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import System.ReadEnvVar (lookupEnv)
import Lib (login)

main :: IO ()
main = do
  username <- lookupEnv "NICONICO_USERNAME"
  password <- lookupEnv "NICONICO_PASSWORD"
  let login' = username >>= \u->
               password <&> \p->
               login u p >>= \cookie->
               putStrLn $ show cookie

  fromMaybe (putStrLn "No env var found") login'
