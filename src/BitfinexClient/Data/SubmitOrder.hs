module BitfinexClient.Data.SubmitOrder
  ( Request (..),
  )
where

import BitfinexClient.Data.Order
import BitfinexClient.Import
import qualified Data.Aeson as A
import Data.Aeson.Lens

data Request
  = Request
      { action :: ExchangeAction,
        amount :: MoneyAmount,
        symbol :: CurrencyPair,
        rate :: ExchangeRate,
        flags :: Set OrderFlag
      }
  deriving (Eq, Ord, Show)

instance ToJSON Request where
  toJSON x =
    A.object
      [ "type"
          A..= ("EXCHANGE LIMIT" :: Text),
        "amount"
          A..= toTextParam (amount x),
        "symbol"
          A..= toTextParam (symbol x),
        "price"
          A..= toTextParam (rate x),
        "flags"
          A..= unOrderFlagSet (flags x)
      ]

instance FromRpc 'SubmitOrder Request Order where
  fromRpc req res@(RawResponse raw) = do
    id0 <-
      maybeToRight
        (failure "OrderId is missing")
        $ raw ^? nth 4 . nth 0 . nth 0 . _Integer
    ss0 <-
      maybeToRight
        (failure "OrderStatus is missing")
        $ raw ^? nth 4 . nth 0 . nth 13 . _String
    ss1 <-
      first failure $
        newOrderStatus ss0
    pure
      Order
        { orderId = OrderId id0,
          orderAction = action req,
          orderAmount = amount req,
          orderSymbol = symbol req,
          orderRate = rate req,
          orderStatus = ss1
        }
    where
      failure =
        fromRpcError SubmitOrder res
