{-# LANGUAGE OverloadedStrings #-}

module Daemon
    ( daemonLoop
    )
where

import           Control.Concurrent             ( threadDelay )
import           Control.Monad                  ( forever
                                                , when
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Data.Maybe                     ( fromJust )
import           Database.Persist.Class         ( insertMany )
import           NRQL.Client                    ( ArchiveFunction(..)
                                                , getAccount
                                                , getAccountHostsCount
                                                , getAccounts
                                                , getHosts
                                                , nrqlLimit
                                                )

import           Config                         ( DaemonT )
import           Logger                         ( Severity(..)
                                                , katipAddNamespace
                                                , logLocM
                                                , logStr
                                                )
import           Types.Account                  ( Account(..)
                                                , Accounts(..)
                                                )
import           Types.Host                     ( Hosts(hList) )
import           Persist                        ( runDB )


accountStep = 1000
daemonLoop :: DaemonT IO ()
daemonLoop = forever $ do
    liftIO $ threadDelay 5000000
    katipAddNamespace "daemon" pipeline

wait :: DaemonT IO ()
wait = liftIO $ threadDelay 3000000

pipeline :: DaemonT IO ()
pipeline = do

    minAccount <- getAccount Min
    logLocM DebugS (logStr $ "min account retrieved: " ++ show minAccount)
    wait

    maxAccount <- getAccount Max
    logLocM DebugS (logStr $ "max account retrieved: " ++ show maxAccount)
    wait

    processAccounts maxAccount minAccount
    return ()

nextTop :: Account -> Account
nextTop account = Account (accNumber account + nrqlLimit)

processAccounts :: Account -> Account -> DaemonT IO ()
processAccounts max bottom = do
    let top = nextTop bottom
    logLocM
        DebugS
        (logStr $ "starting pipeline from " ++ show bottom ++ " to " ++ show top
        )
    accounts <- getAccounts bottom top
    wait
    processAccountHosts accounts
    when (top < max) $ processAccounts max top

processAccountHosts :: Accounts -> DaemonT IO ()
processAccountHosts (Accounts []      ) = return ()
processAccountHosts (Accounts (a : as)) = do
    hostsCount <- getAccountHostsCount a
    wait
    -- if hostsCount <= 1000 then getHosts a else return ()
    hosts <- getHosts a
    wait
    runDB (insertMany $ hList hosts)
    processAccountHosts (Accounts as)

-- λ ➜ curl -v -H 'Accept: application/json' -H "Content-Type: application/json" --data "{\"jsonVersion\":1,\"query\":\"SELECT latest(entityId), latest(linuxDistribution), latest(agentVersion), latest(kernelVersion), latest(instanceType), latest(operatingSystem), latest(windowsVersion), latest(windowsPlatform), latest(windowsFamily), latest(coreCount), latest(processorCount), latest(systemMemoryBytes) FROM SystemSample facet entityId LIMIT 10 SINCE 1 week ago\",\"metadata\":{\"source\":\"NRI-CUSTOMERS\"},\"account\":1,\"format\":\"json\"}" http://dirac.nr-ops.net:8084/api/1/query | jq .
