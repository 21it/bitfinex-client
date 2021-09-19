{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Data.Type
  ( -- * Orders
    -- $orders
    OrderId (..),
    OrderClientId (..),
    OrderGroupId (..),
    Order (..),
    OrderFlag (..),
    OrderFlagAcc (..),
    unOrderFlag,
    unOrderFlagSet,
    OrderStatus (..),
    newOrderStatus,

    -- * Trading
    -- $trading
    ExchangeAction (..),
    newExchangeAction,
    ExchangeRate (..),
    newExchangeRate,
    FeeRate (..),
    newFeeRate,
    RebateRate (..),
    ProfitRate (..),
    newProfitRate,
    MoneyAmount (..),
    newMoneyAmount,
    newRawAmt,
    CurrencyCode (..),
    CurrencyPair,
    currencyPairBase,
    currencyPairQuote,
    newCurrencyPair,
    newCurrencyPair',

    -- * Misc
    -- $misc
    PosRat,
    unPosRat,
    newPosRat,
    subPosRat,
    Error (..),
  )
where

import BitfinexClient.Class.ToRequestParam
import BitfinexClient.Data.Kind
import BitfinexClient.Import.External
import qualified Data.Text as T
import qualified Network.HTTP.Client as Web

-- $orders
-- Order data received from Bitfinex
-- and types related to orders.

newtype OrderId
  = OrderId Natural
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON)

newtype OrderClientId
  = OrderClientId Natural
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON)

newtype OrderGroupId
  = OrderGroupId Natural
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON)

data Order (a :: Location)
  = Order
      { orderId :: OrderId,
        orderAction :: ExchangeAction,
        orderAmount :: MoneyAmount,
        orderSymbol :: CurrencyPair,
        orderRate :: ExchangeRate,
        orderStatus :: OrderStatus
      }
  deriving (Eq, Ord, Show)

data OrderFlag
  = Hidden
  | Close
  | ReduceOnly
  | PostOnly
  | Oco
  | NoVarRates
  deriving (Eq, Ord, Show)

newtype OrderFlagAcc
  = OrderFlagAcc Natural
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
  | PostOnlyCanceled
  | RsnDust
  | RsnPause
  deriving (Eq, Ord, Show)

newOrderStatus :: Text -> Either Text OrderStatus
newOrderStatus = \case
  "ACTIVE" -> Right Active
  x | "EXECUTED" `T.isPrefixOf` x -> Right Executed
  x | "PARTIALLY FILLED" `T.isPrefixOf` x -> Right PartiallyFilled
  x | "INSUFFICIENT MARGIN" `T.isPrefixOf` x -> Right InsufficientMargin
  "CANCELED" -> Right Canceled
  "POSTONLY CANCELED" -> Right PostOnlyCanceled
  "RSN_DUST" -> Right RsnDust
  "RSN_PAUSE" -> Right RsnPause
  _ -> Left "OrderStatus is not recognized"

-- $trading
-- Data related to trading and money.

data ExchangeAction
  = Buy
  | Sell
  deriving (Eq, Ord, Show)

newExchangeAction :: Rational -> Either Error ExchangeAction
newExchangeAction x
  | x > 0 = Right Buy
  | x < 0 = Right Sell
  | otherwise =
    Left $
      ErrorSmartCon "ExchangeAction can not be derived from zero amount"

newtype ExchangeRate
  = ExchangeRate PosRat
  deriving newtype (Eq, Ord, Show, Num, ToRequestParam)

newExchangeRate :: Rational -> Either Error ExchangeRate
newExchangeRate = (ExchangeRate <$>) . newPosRat

newtype FeeRate (a :: MarketRelation)
  = FeeRate PosRat
  deriving newtype (Eq, Ord, Show, Num)

newFeeRate :: Rational -> Either Error (FeeRate a)
newFeeRate = (FeeRate <$>) . newPosRat

newtype RebateRate (a :: MarketRelation)
  = RebateRate Rational
  deriving newtype (Eq, Ord, Show, Num)

newtype ProfitRate
  = ProfitRate {unProfitRate :: PosRat}
  deriving newtype (Eq, Ord, Show, Num)

newProfitRate :: Rational -> Either Error ProfitRate
newProfitRate = (ProfitRate <$>) . newPosRat

newtype MoneyAmount
  = MoneyAmount {unMoneyAmount :: PosRat}
  deriving newtype (Eq, Ord, Show, Num, ToRequestParam)

newMoneyAmount :: Rational -> Either Error MoneyAmount
newMoneyAmount = (MoneyAmount <$>) . newPosRat

newRawAmt :: ExchangeAction -> MoneyAmount -> Rational
newRawAmt act amt =
  case act of
    Buy -> absAmt
    Sell -> (-1) * absAmt
  where
    absAmt = abs . unPosRat $ coerce amt

newtype CurrencyCode (a :: CurrencyRelation)
  = CurrencyCode Text
  deriving newtype (Eq, Ord, Show, IsString)

data CurrencyPair
  = CurrencyPair
      { currencyPairBase :: CurrencyCode 'Base,
        currencyPairQuote :: CurrencyCode 'Quote
      }
  deriving (Eq, Ord, Show)

instance ToRequestParam CurrencyPair where
  toTextParam x =
    "t"
      <> (coerce $ currencyPairBase x :: Text)
      <> (coerce $ currencyPairQuote x :: Text)

newCurrencyPair ::
  CurrencyCode 'Base ->
  CurrencyCode 'Quote ->
  Either Error CurrencyPair
newCurrencyPair base quote =
  if (coerce base :: Text) == coerce quote
    then
      Left . ErrorSmartCon $
        "CurrencyPair should not be the identical but got base "
          <> show base
          <> " and quote "
          <> show quote
    else
      Right $
        CurrencyPair base quote

newCurrencyPair' :: Text -> Either Error CurrencyPair
newCurrencyPair' raw =
  if (length raw == 7) && (prefix == "t")
    then newCurrencyPair (CurrencyCode base0) (CurrencyCode quote0)
    else Left . ErrorSmartCon $ "Invalid CurrencyPair " <> raw
  where
    (prefix, xs) = T.splitAt 1 raw
    (base0, quote0) = T.splitAt 3 xs

-- $misc
-- General utility data used elsewhere.

newtype PosRat
  = PosRat {unPosRat :: Rational}
  deriving newtype (Eq, Ord, Show, Num, ToRequestParam)

newPosRat :: Rational -> Either Error PosRat
newPosRat x
  | x > 0 = Right $ PosRat x
  | otherwise =
    Left . ErrorSmartCon $ "PosRat should be positive, but got " <> show x

subPosRat :: PosRat -> PosRat -> Either Error PosRat
subPosRat x0 x1 = newPosRat (coerce x0 - coerce x1)

data Error
  = ErrorWebException HttpException
  | ErrorWebPub Web.Request (Web.Response ByteString)
  | ErrorWebPrv ByteString Web.Request (Web.Response ByteString)
  | ErrorFromRpc Text
  | ErrorSmartCon Text
  | ErrorMissingOrder OrderId
  | ErrorUnverifiedOrder (Order 'Local) (Order 'Remote)
  | ErrorOrderStatus (Order 'Remote)
  deriving (Show)
