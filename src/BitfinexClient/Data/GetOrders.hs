{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Data.GetOrders
  ( Request (..),
  )
where

import BitfinexClient.Class.FromRpc
import BitfinexClient.Data.Kind
import BitfinexClient.Data.Type
import BitfinexClient.Import

data Request
  = Request
      { currencyPair :: CurrencyPair,
        orderIds :: Set OrderId
      }
  deriving (Eq, Ord, Show)

instance ToJSON Request where
  toJSON = toJSON . orderIds

instance FromRpc 'RetrieveOrders Request (Map OrderId (Order 'Remote)) where
  fromRpc = parser

instance FromRpc 'OrdersHistory Request (Map OrderId (Order 'Remote)) where
  fromRpc = parser

parser ::
  Request ->
  RawResponse ->
  Either Error (Map OrderId (Order 'Remote))
parser _ (RawResponse raw) = do
  --
  -- TODO : use req
  --
  parseOrderMap raw
