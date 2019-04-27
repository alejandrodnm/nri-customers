{-# LANGUAGE OverloadedStrings #-}

module NRQLSpec where

import           Control.Monad.Catch            ( SomeException )
import           Control.Monad.Reader           ( runReaderT )
import           Control.Monad.Trans.Except     ( runExceptT )
import           Data.Aeson
import           Network.HTTP.Req               ( JsonResponse )
import           Test.Hspec

import           Config
import           Types.Account
import           Types.Host
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
        it "decodes min"
            $ (decode "{\"results\":[{\"min\": 3.0}]}" :: Maybe Account)
            `shouldBe` Just (Account 3)
        it "decodes max"
            $ (decode "{\"results\":[{\"max\": 3.0}]}" :: Maybe Account)
            `shouldBe` Just (Account 3)
    it "decode accounts"
        $ (decode "{\"results\":[{\"members\": [33.0, 34.0]}]}" :: Maybe
                Accounts
          )
        `shouldBe` Just (Accounts [Account 33, Account 34])
    it "decode hostscount"
        $ (decode "{\"results\":[{\"uniqueCount\":5}]}" :: Maybe HostsCount)
        `shouldBe` Just (HostsCount 5)
    it "decode host"
        $ let
              rawHost
                  = "{\"name\":\"974205676942181148\",\"results\":[{\"latest\":\"CentOSLinux7(Core)\"},{\"latest\":\"1.3.20\"},{\"latest\":\"3.10.0-514.21.1.el7.x86_64\"},{\"latest\":\"unknown\"},{\"latest\":\"linux\"},{\"latest\":null},{\"latest\":null},{\"latest\":null},{\"latest\":\"14\"},{\"latest\":\"56\"},{\"latest\":\"269033377792\"}]}"
          in  (decode rawHost :: Maybe Host) `shouldBe` Just
                  (Host "974205676942181148"
                        (Just "CentOSLinux7(Core)")
                        (Just "1.3.20")
                        (Just "3.10.0-514.21.1.el7.x86_64")
                        (Just "unknown")
                        (Just "linux")
                        Nothing
                        Nothing
                        Nothing
                        (Just 14)
                        (Just 56)
                        (Just 269033377792)
                        Nothing
                  )

    it "decode hosts"
        $ let
              rawHosts
                  = "{\"facets\":[{\"name\":\"974205676942181148\",\"results\":[{\"latest\":\"CentOSLinux7(Core)\"},{\"latest\":\"1.3.20\"},{\"latest\":\"3.10.0-514.21.1.el7.x86_64\"},{\"latest\":\"unknown\"},{\"latest\":\"linux\"},{\"latest\":null},{\"latest\":null},{\"latest\":null},{\"latest\":\"14\"},{\"latest\":\"56\"},{\"latest\":\"269033377792\"}]},{\"name\":\"8923018336323236291\",\"results\":[{\"latest\":\"ContainerLinuxbyCoreOS1911.5.0(Rhyolite)\"},{\"latest\":\"1.0.1019\"},{\"latest\":\"4.14.84-coreos\"},{\"latest\":\"SupermicroSYS-6019U-TN4R4T\"},{\"latest\":\"linux\"},{\"latest\":null},{\"latest\":null},{\"latest\":null},{\"latest\":\"18\"},{\"latest\":\"72\"},{\"latest\":\"404430925824\"}]}]}"
          in  (decode rawHosts :: Maybe Hosts) `shouldBe` Just
                  (Hosts
                      [ Host "974205676942181148"
                             (Just "CentOSLinux7(Core)")
                             (Just "1.3.20")
                             (Just "3.10.0-514.21.1.el7.x86_64")
                             (Just "unknown")
                             (Just "linux")
                             Nothing
                             Nothing
                             Nothing
                             (Just 14)
                             (Just 56)
                             (Just 269033377792)
                             Nothing
                      , Host
                          "8923018336323236291"
                          (Just "ContainerLinuxbyCoreOS1911.5.0(Rhyolite)")
                          (Just "1.0.1019")
                          (Just "4.14.84-coreos")
                          (Just "SupermicroSYS-6019U-TN4R4T")
                          (Just "linux")
                          Nothing
                          Nothing
                          Nothing
                          (Just 18)
                          (Just 72)
                          (Just 404430925824)
                          Nothing
                      ]
                  )
