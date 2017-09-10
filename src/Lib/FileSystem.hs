module Lib.FileSystem where

import System.Directory (doesDirectoryExist, createDirectory)

prepareDirectory :: String -> IO ()
prepareDirectory path =
  doesDirectoryExist path >>= \exist->
  if exist
    then return ()
    else createDirectory path

