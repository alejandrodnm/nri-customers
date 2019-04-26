{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DataKinds         #-}

module Models.Account where

import           Data.Text                      ( Text )
import           Database.Persist.TH            ( mkMigrate
                                                , mkPersist
                                                , persistLowerCase
                                                , persistLowerCase
                                                , share
                                                , sqlSettings
                                                )

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    Host
        entityId Text
        linuxDistribution String Maybe
        agentVersion      String Maybe
        kernelVersion     String Maybe
        instanceType      String Maybe
        operatingSystem   String Maybe
        windowsVersion    String Maybe
        windowsPlatform   String Maybe
        windowsFamily     String Maybe
        coreCount         Int Maybe
        processorCount    Int Maybe
        systemMemoryBytes Int Maybe
        deriving Show Eq
|]
