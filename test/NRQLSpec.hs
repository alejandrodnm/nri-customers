{-# LANGUAGE OverloadedStrings #-}

module NRQLSpec where

import           Test.Hspec
import           NRQL
import           Config
import           Control.Monad.Reader           ( runReaderT )
import           Control.Monad.Trans.Except     ( runExceptT )
import           Control.Monad.Catch            ( SomeException )
import           Data.Aeson
import           Network.HTTP.Req               ( JsonResponse )

-- config :: AppConfig
-- config = AppConfig { cfgLogEnv       = undefined
--                    , cfgLogContext   = undefined
--                    , cfgLogNamespace = undefined
--                    , cfgPort         = undefined
--                    , cfgEnv          = undefined
--                    , cfgDBPool       = undefined
--                    , cfgNREndpoint   = "nr.com"
--                    }

-- spec :: Spec
-- spec = it "failed request" $ do
--     let query = Query "" "" "" ""
--         body  = requestBody query
--     a <- runReaderT
--         (runDaemonT
--             (request' body :: DaemonT
--                   IO
--                   (Either SomeException (JsonResponse Integer))
--             )
--         )
--         config
--     case a of
--         Left e -> show e `shouldBe` ""
