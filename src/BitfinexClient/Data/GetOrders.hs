{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Data.GetOrders
  ( Options (..),
    optsDef,
    optsSym,
    optsIds,
  )
where

import BitfinexClient.Class.FromRpc
import BitfinexClient.Data.Kind
import BitfinexClient.Data.Type
import BitfinexClient.Import

data Options
  = Options
      { currencyPair :: Maybe CurrencyPair,
        orderIds :: Set OrderId
      }
  deriving (Eq, Ord, Show)

optsDef :: Options
optsDef =
  Options
    { currencyPair = Nothing,
      orderIds = mempty
    }

optsSym :: CurrencyPair -> Options
optsSym sym = optsDef {currencyPair = Just sym}

optsIds :: Set OrderId -> Options
optsIds ids = optsDef {orderIds = ids}

instance ToJSON Options where
  toJSON = toJSON . orderIds

instance FromRpc 'RetrieveOrders Options (Map OrderId (Order 'Remote)) where
  fromRpc = parser

instance FromRpc 'OrdersHistory Options (Map OrderId (Order 'Remote)) where
  fromRpc = parser

parser ::
  Options ->
  RawResponse ->
  Either Error (Map OrderId (Order 'Remote))
parser _ (RawResponse raw) = do
  --
  -- TODO : use req
  --
  parseOrderMap raw
