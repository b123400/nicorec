module Lib.Conduit where

import Conduit ( ConduitT
               , MonadIO
               , ResourceT
               , (.|)
               , await
               , liftIO
               , yield
               , runResourceT
               , runConduit )
import Control.Concurrent.STM.TMQueue (TMQueue, newTMQueue, readTMQueue, closeTMQueue)
import Control.Monad.STM (atomically)
import Control.Monad.Loops (whileJust_)
import Control.Monad.Catch (catchAll)
import Control.Concurrent (forkIO)
import Data.Conduit.TQueue (sinkTMQueue)
import Data.Monoid ((<>))

import Lib.Operators ((>>=?))
import Lib.Utility (retry)


dedup :: Eq a => Monad m => Int -> ConduitT a a m ()
dedup limit = dedup' limit []
dedup' limit existing =
  await >>=? \val->
  let isDup       = elem val existing
      newExisting = if isDup
                    then existing
                    else drop (length existing + 1 - limit) (existing <> [val])
  in (if not isDup then yield val else return ())
     >> dedup' limit newExisting

fork :: MonadIO m => ConduitT (ConduitT () a (ResourceT IO) ()) (TMQueue a) m ()
fork = whileJust_ await $ \source->
         (liftIO $ atomically newTMQueue)
         >>= \q-> (liftIO $ forkIO
                          $ (retry 3 $ runResourceT
                                     $ runConduit
                                     $ source .| sinkTMQueue q)
                          `catchAll`
                           (\e-> (putStrLn $ show e)
                              >> (atomically $ closeTMQueue q)))
         >> yield q

collect :: MonadIO m => ConduitT (TMQueue a) a m ()
collect = whileJust_ await drainAll
  where drainAll q = whileJust_ (liftIO $ atomically $ readTMQueue q) yield
