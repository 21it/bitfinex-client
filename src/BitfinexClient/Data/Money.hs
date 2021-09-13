module BitfinexClient.Data.Money
  ( ExchangeAction (..),
    rawAmt2ExchangeAction,
    ExchangeRate,
    newExchangeRate,
    FeeRate,
    newFeeRate,
    RebateRate (..),
    MoneyAmount (..),
    newMoneyAmount,
    newRawAmt,
    CurrencyCode (..),
    CurrencyPair,
    currencyPairBase,
    currencyPairQuote,
    newCurrencyPair,
  )
where

import BitfinexClient.Class.ToRequestParam
import BitfinexClient.Data.Kind
import BitfinexClient.Data.Type
import BitfinexClient.Import.External

data ExchangeAction
  = Buy
  | Sell
  deriving (Eq, Ord, Show)

rawAmt2ExchangeAction :: Rational -> Either Error ExchangeAction
rawAmt2ExchangeAction x
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

newtype MoneyAmount
  = MoneyAmount PosRat
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
