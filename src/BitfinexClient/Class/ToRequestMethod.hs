module BitfinexClient.Class.ToRequestMethod
  ( ToRequestMethod (..),
  )
where

import BitfinexClient.Data.Kind
import BitfinexClient.Data.Type

class ToRequestMethod (method :: Method) where
  toRequestMethod :: Rpc (method :: Method) -> RequestMethod

instance ToRequestMethod 'MarketAveragePrice where
  toRequestMethod Rpc = POST

instance ToRequestMethod 'SubmitOrder where
  toRequestMethod Rpc = POST

instance ToRequestMethod 'FeeSummary where
  toRequestMethod Rpc = POST
