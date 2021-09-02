module BitfinexClient.Class.ToBaseUrl
  ( ToBaseUrl (..),
  )
where

import BitfinexClient.Data.Kind
import BitfinexClient.Data.Type

class ToBaseUrl (method :: Method) where
  toBaseUrl :: Rpc (method :: Method) -> BaseUrl

instance ToBaseUrl 'MarketAveragePrice where
  toBaseUrl Rpc = prv

instance ToBaseUrl 'SubmitOrder where
  toBaseUrl Rpc = prv

instance ToBaseUrl 'FeeSummary where
  toBaseUrl Rpc = prv

prv :: BaseUrl
prv = "https://api.bitfinex.com"
