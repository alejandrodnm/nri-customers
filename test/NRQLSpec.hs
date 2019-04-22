{-# LANGUAGE OverloadedStrings #-}

module NRQLSpec where

import           Config
import           Control.Monad.Catch        (SomeException)
import           Control.Monad.Reader       (runReaderT)
import           Control.Monad.Trans.Except (runExceptT)
import           Data.Aeson
import           Models.Account
import           Network.HTTP.Req           (JsonResponse)
import           Test.Hspec

-- config :: AppConfig
-- config = AppConfig { cfgLogEnv       = undefined
--                    , cfgLogContext   = undefined
--                    , cfgLogNamespace = undefined
--                    , cfgPort         = undefined
--                    , cfgEnv          = undefined
--                    , cfgDBPool       = undefined
--                    , cfgNREndpoint   = "nr.com"
--                    }

spec :: Spec
spec = do
    context "decode account" $ do
        it "decodes min" $
            (decode "{\"results\":[{\"min\": 3.0}]}" :: Maybe Account)
                `shouldBe` Just (Account 3)
        it "decodes max" $
            (decode "{\"results\":[{\"max\": 3.0}]}" :: Maybe Account)
                `shouldBe` Just (Account 3)
    it "decode accounts" $
        (decode "{\"results\":[{\"members\": [33.0, 34.0]}]}" :: Maybe Accounts)
            `shouldBe` Just (Accounts [Account 33, Account 34])
