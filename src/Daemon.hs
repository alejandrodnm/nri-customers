{-# LANGUAGE OverloadedStrings #-}

module Daemon
    ( daemonLoop
    )
where

import           Config                 (DaemonT)
import           Control.Concurrent     (threadDelay)
import           Control.Monad          (forever, when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Maybe             (fromJust)
import           Logger                 (Severity (..), katipAddNamespace,
                                         logLocM, logStr)
import           Models.Account         (Account (..), Accounts (..))
import           NRQL                   (ArchiveFunction (..), getAccount,
                                         getAccountEntitiesCount, getAccounts,
                                         nrqlLimit)

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
    logLocM DebugS
        (logStr $ "starting pipeline from " ++ show bottom ++ " to " ++
            show top)
    accounts <- getAccounts bottom top
    wait
    processAccountEntities accounts
    when (top < max) $ processAccounts max top

processAccountEntities :: Accounts -> DaemonT IO ()
processAccountEntities (Accounts []) = return ()
processAccountEntities (Accounts (a:as)) = do
    entitiesCount <- getAccountEntitiesCount a
    liftIO $ print entitiesCount
    processAccountEntities (Accounts as)

-- λ ➜ curl -v -H 'Accept: application/json' -H "Content-Type: application/json" --data "{\"jsonVersion\":1,\"query\":\"SELECT latest(entityId), latest(linuxDistribution), latest(agentVersion), latest(kernelVersion), latest(instanceType), latest(operatingSystem), latest(windowsVersion), latest(windowsPlatform), latest(windowsFamily), latest(coreCount), latest(processorCount), latest(systemMemoryBytes) FROM SystemSample facet entityId LIMIT 10 SINCE 1 week ago\",\"metadata\":{\"source\":\"NRI-CUSTOMERS\"},\"account\":1,\"format\":\"json\"}" http://dirac.nr-ops.net:8084/api/1/query | jq .
