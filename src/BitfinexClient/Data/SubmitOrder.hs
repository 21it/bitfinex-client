module BitfinexClient.Data.SubmitOrder
  ( Request (..),
    Options (..),
    optsDef,
    optsPostOnly,
  )
where

import BitfinexClient.Import
import qualified Data.Aeson as A
import Data.Aeson.Lens

data Request
  = Request
      { action :: ExchangeAction,
        amount :: MoneyAmount,
        symbol :: CurrencyPair,
        rate :: ExchangeRate,
        options :: Options
      }
  deriving (Eq, Ord, Show)

data Options
  = Options
      { clientId :: Maybe OrderClientId,
        groupId :: Maybe OrderGroupId,
        flags :: Set OrderFlag
      }
  deriving (Eq, Ord, Show)

optsDef :: Options
optsDef =
  Options
    { clientId = Nothing,
      groupId = Nothing,
      flags = mempty
    }

optsPostOnly :: Options
optsPostOnly =
  Options
    { clientId = Nothing,
      groupId = Nothing,
      flags = [PostOnly]
    }

instance ToJSON Request where
  toJSON req =
    A.object
      [ "gid"
          A..= groupId opts,
        "cid"
          A..= clientId opts,
        "type"
          A..= ("EXCHANGE LIMIT" :: Text),
        "amount"
          A..= toTextParam (amount req),
        "symbol"
          A..= toTextParam (symbol req),
        "price"
          A..= toTextParam (rate req),
        "flags"
          A..= unOrderFlagSet (flags opts)
      ]
    where
      opts = options req

instance FromRpc 'SubmitOrder Request Order where
  fromRpc req res@(RawResponse raw) = do
    id0 <-
      maybeToRight
        (failure "OrderId is missing")
        $ raw ^? nth 4 . nth 0 . nth 0 . _Integral
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
