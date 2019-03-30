{-# LANGUAGE OverloadedStrings #-}

module Daemon
    ( daemonLoop
    )
where

import           Logger                         ( katipAddNamespace )
import           Config                         ( DaemonT )
import           Control.Concurrent             ( threadDelay )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Data.Maybe                     ( fromJust )
import           Katip                          ( logStr )
import           NRQL                           ( nrqlPipeline )
import           Control.Monad                  ( forever )

daemonLoop :: DaemonT IO ()
daemonLoop = forever $ do
    liftIO $ threadDelay 5000000
    katipAddNamespace "daemon" nrqlPipeline
