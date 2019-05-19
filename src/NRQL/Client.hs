{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}

module NRQL.Client where

import           Control.Monad.Reader           ( MonadReader
                                                , asks
                                                )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Proxy
import           Data.Typeable                  ( typeOf )
import           GHC.Generics                   ( Generic )

import           Config                         ( AppConfig
                                                , cfgNREndpoint
                                                )
import           Network.HTTP.Req.Client        ( ReqClient(..) )
import           NRQL.Query                     ( Query
                                                , encodeQuery
                                                )
import           Types.Account                  ( Account(..)
                                                , Accounts
                                                )
import           Types.Host                     ( Host(..)
                                                , Hosts(..)
                                                , HostsCount(..)
                                                )

data DiracRequest = DiracRequest
    { query       :: String
    , account     :: Account
    , format      :: String
    , jsonVersion :: Integer
    , metadata    :: DiracMetadata
    } deriving(Generic, Show)

instance ToJSON DiracRequest

newtype DiracMetadata = DiracMetadata
    { source :: String
    } deriving(Generic, Show)

instance ToJSON DiracMetadata
instance FromJSON DiracMetadata

metaAccount :: Account
metaAccount = Account 313870

requestBody :: Query -> Account -> DiracRequest
requestBody q acc = DiracRequest { query       = encodeQuery q
                                 , account     = acc
                                 , format      = "json"
                                 , jsonVersion = 1
                                 , metadata    = DiracMetadata "NRI-CUSTOMERS"
                                 }

runQuery
    :: (FromJSON response, MonadReader AppConfig m, ReqClient m)
    => Proxy response
    -> Account
    -> Query
    -> m response
runQuery proxy acc query = do
    let body = requestBody query acc
    (url, options) <- asks cfgNREndpoint
    reqRequest proxy url options body
