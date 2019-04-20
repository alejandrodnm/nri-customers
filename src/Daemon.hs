{-# LANGUAGE OverloadedStrings #-}

module Daemon
    ( daemonLoop
    )
where

import           Config                 (DaemonT)
import           Control.Concurrent     (threadDelay)
import           Control.Monad          (forever)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Maybe             (fromJust)
import           Katip                  (logStr)
import           Logger                 (katipAddNamespace)
import           NRQL                   (ArchiveFunction (..), getAccount)

daemonLoop :: DaemonT IO ()
daemonLoop = forever $ do
    liftIO $ threadDelay 5000000
    katipAddNamespace "daemon" pipeline


pipeline :: DaemonT IO ()
pipeline = do

    minAccount <- getAccount Min
    liftIO $ threadDelay 1000000

    maxAccount <- getAccount Max
    liftIO $ threadDelay 1000000

    return ()
