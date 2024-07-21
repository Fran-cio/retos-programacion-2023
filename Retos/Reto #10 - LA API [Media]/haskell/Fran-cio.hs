module Fran_cio where
-- ```
-- /*
--  * Llamar a una API es una de las tareas más comunes en programación.
--  *
--  * Implementa una llamada HTTP a una API (la que tú quieras) y muestra su
--  * resultado a través de la terminal. Por ejemplo: Pokémon, Marvel...
--  *
--  * Aquí tienes un listado de posibles APIs: 
--  * https://github.com/public-apis/public-apis
--  */
-- ```

{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Conduit (simpleHttp)
import Data.Aeson (decode, (.:), withObject, eitherDecode)
import Data.Aeson.Types (parseMaybe)
import Data.Text (Text, pack)
import qualified Data.ByteString.Lazy as B
import Data.Aeson.Key (fromString)

-- Define a data type to hold the extracted information
data EthereumInfo = EthereumInfo
    { rank :: Text
    , symbol :: Text
    , name :: Text
    , supply :: Text
    , marketCapUsd :: Text
    , volumeUsd24Hr :: Text
    , priceUsd :: Text
    , changePercent24Hr :: Text
    } deriving (Show)

-- Parse the JSON response
parseEthereumInfo :: B.ByteString -> Maybe EthereumInfo
parseEthereumInfo jsonStr = do
    obj <- decode jsonStr
    flip parseMaybe obj $ \o -> do
        dataObj <- o .: fromString "data"
        EthereumInfo <$> dataObj .: fromString "rank"
                     <*> dataObj .: fromString "symbol"
                     <*> dataObj .: fromString "name"
                     <*> dataObj .: fromString "supply"
                     <*> dataObj .: fromString "marketCapUsd"
                     <*> dataObj .: fromString "volumeUsd24Hr"
                     <*> dataObj .: fromString "priceUsd"
                     <*> dataObj .: fromString "changePercent24Hr"

getData :: [Char] -> IO ()
getData crypto = do
    response <- simpleHttp ("https://api.coincap.io/v2/assets/" ++ crypto)
    let ethInfo = parseEthereumInfo response
    case ethInfo of
        Just info -> print (name info <> pack " vale " <> priceUsd info <> pack " Dolares")
        Nothing -> putStrLn "Failed to parse JSON"
main :: IO ()
main = mapM_ getData ["ethereum", "bitcoin", "xrp", "cardano", "polkadot", "pancakeswap", "usd-coin"]

-- Expected Output:
-- "Ethereum vale 3504.6360230867244981 Dolares"
-- "Bitcoin vale 67770.9710702966227356 Dolares"
-- "XRP vale 0.5969277065072313 Dolares"
-- "Cardano vale 0.4347680706336956 Dolares"
-- "Polkadot vale 6.3782723395022943 Dolares"
-- "PancakeSwap vale 2.0765406278441906 Dolares"
-- "USDC vale 0.9997097928250468 Dolares"
-- >>> main
