module Lib.FileSystem where

import Data.Maybe (Maybe(..), maybe)
import System.Directory (doesDirectoryExist, createDirectory)
import System.FilePath.Posix (joinPath, splitPath)

prepareDirectory :: String -> IO ()
prepareDirectory path =
  maybe (return ()) prepareDirectory (parent path)
  >> doesDirectoryExist path
  >>= \exist-> if exist
               then return ()
               else createDirectory path

parent :: String -> Maybe String
parent path =
  case splitPath path of
    []      -> Nothing
    [_]     -> Nothing -- Root layer should be ignored
    folders -> Just $ joinPath $ init folders
