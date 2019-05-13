module NRQL.Aeson
    ( foldParsers
    , responseResults
    , fromResults
    )
where

import           Data.Aeson                     ( FromJSON(..)
                                                , Object
                                                , ToJSON(..)
                                                , Value
                                                , Value(Array, Object)
                                                , (.:)
                                                , (.:?)
                                                )
import           Data.Aeson.Types               ( Parser )
import           Data.Text                      ( Text )
import qualified Data.Vector                   as V

-- Returns the first valid parser, if no parser is valid, it fails.
foldParsers :: (FromJSON b) => [Parser (Maybe b)] -> Parser b
foldParsers []       = fail "none of the allowed functions attributes was found"
foldParsers (p : ps) = p >>= maybe (foldParsers ps) return

-- Returns a parser up to the `results` attribute of the response
responseResults :: Value -> Parser (V.Vector Value)
responseResults v = case v of
    Object o -> do
        r <- o .: "results"
        case r of
            Array v -> return v
            _       -> fail "array of results expected"
    _ -> fail "root object expected"

fromResults :: (FromJSON b) => V.Vector Value -> Text -> Parser (Maybe b)
fromResults v f = case length v of
    0 -> fail "results array from query is empty"
    _ -> case V.head v of
        Object m -> m .:? f
        _        -> fail "array of result objects expected"
