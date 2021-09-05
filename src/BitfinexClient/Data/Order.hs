module BitfinexClient.Data.Order
  ( Order (..),
    OrderId (..),
    OrderFlag (..),
    OrderFlagAcc (..),
    unOrderFlag,
    unOrderFlagSet,
    OrderStatus (..),
    newOrderStatus,
  )
where

import BitfinexClient.Data.Money
import BitfinexClient.Data.Type
import BitfinexClient.Import.External
import qualified Data.Text as T

data Order
  = Order
      { orderId :: OrderId,
        orderRate :: ExchangeRate,
        orderAmount :: Rational,
        orderStatus :: OrderStatus
      }
  deriving (Eq, Ord, Show)

newtype OrderId
  = OrderId Integer
  deriving newtype (Eq, Ord, Show)

data OrderFlag
  = Hidden
  | Close
  | ReduceOnly
  | PostOnly
  | Oco
  | NoVarRates
  deriving (Eq, Ord, Show)

newtype OrderFlagAcc
  = OrderFlagAcc Integer
  deriving newtype (Eq, Ord, Show, Num, ToJSON)

unOrderFlag :: OrderFlag -> OrderFlagAcc
unOrderFlag = OrderFlagAcc . \case
  Hidden -> 64
  Close -> 512
  ReduceOnly -> 1024
  PostOnly -> 4096
  Oco -> 16384
  NoVarRates -> 524288

unOrderFlagSet :: Set OrderFlag -> OrderFlagAcc
unOrderFlagSet =
  foldr (\x acc -> acc + unOrderFlag x) $ OrderFlagAcc 0

data OrderStatus
  = Active
  | Executed
  | PartiallyFilled
  | InsufficientMargin
  | Canceled
  | RsnDust
  | RsnPause
  deriving (Eq, Ord, Show)

newOrderStatus :: Text -> Either Error OrderStatus
newOrderStatus = \case
  "ACTIVE" -> Right Active
  x | "EXECUTED" `T.isPrefixOf` x -> Right Executed
  x | "PARTIALLY FILLED" `T.isPrefixOf` x -> Right PartiallyFilled
  x | "INSUFFICIENT MARGIN" `T.isPrefixOf` x -> Right InsufficientMargin
  "CANCELED" -> Right Canceled
  "RSN_DUST" -> Right RsnDust
  "RSN_PAUSE" -> Right RsnPause
  x -> Left . ErrorSmartCon $ "OrderStatus is not recognized " <> x
