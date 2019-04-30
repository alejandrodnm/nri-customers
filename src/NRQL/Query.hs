module NRQL.Query where

import           Types.Account                  ( Account(..) )

nrqlLimit :: Int
nrqlLimit = 1000

data Query = Query Select From Where Since deriving (Show)

data ArchiveFunction
    = Min
    | Max
    | Uniques

type Select = String
type From = String
type Where = String
type Since = String

accountQuery :: ArchiveFunction -> Query
accountQuery f =
    let f' = case f of
            Min -> "min(account)"
            Max -> "max(account)"
    in  Query f' "ArchiveFile" "storedEventType = 'SystemSample'" "1 week ago"

accountsQuery :: Account -> Account -> Query
accountsQuery bottom top =
    let where' =
                "storedEventType = 'SystemSample' AND account >= "
                    ++ (show . accNumber) bottom
                    ++ " AND account < "
                    ++ (show . accNumber) top
    in  Query "uniques(account)" "ArchiveFile" where' "1 week ago"

hostsCountQuery :: Query
hostsCountQuery =
    Query "uniqueCount(entityId)" "SystemSample" "true" "1 week ago"

hostsCountQueryFilteredByEntity :: String -> Query
hostsCountQueryFilteredByEntity entityId =
    let where' = "entityId like '" ++ entityId ++ "%'"
    in  Query "uniqueCount(entityId)" "SystemSample" where' "1 week ago"

hostsQuery :: Query
hostsQuery =
    let
        select
            = "latest(linuxDistribution), latest(agentVersion), latest(kernelVersion), latest(instanceType), latest(operatingSystem), latest(windowsVersion), latest(windowsPlatform), latest(windowsFamily), latest(coreCount), latest(processorCount), latest(systemMemoryBytes)"
    in  Query select "SystemSample" "true facet entityId" "1 week ago"

hostsQueryFilteredByEntity :: String -> Query
hostsQueryFilteredByEntity entityId =
    let
        select
            = "latest(linuxDistribution), latest(agentVersion), latest(kernelVersion), latest(instanceType), latest(operatingSystem), latest(windowsVersion), latest(windowsPlatform), latest(windowsFamily), latest(coreCount), latest(processorCount), latest(systemMemoryBytes)"
        where' = "entityId like '" ++ entityId ++ "%' facet entityId"
    in
        Query select "SystemSample" where' "1 week ago"

encodeQuery :: Query -> String
encodeQuery (Query select from where' since) =
    "select "
        ++ select
        ++ " from "
        ++ from
        ++ " where "
        ++ where'
        ++ " since "
        ++ since
        ++ " limit "
        ++ show nrqlLimit
