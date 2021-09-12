module BitfinexClient.Data.Money
  ( ExchangeAction (..),
    rawAmt2ExchangeAction,
    MoneyAmount (..),
    newMoneyAmount,
    newRawAmt,
    CurrencyCode (..),
    CurrencyPair,
    currencyPairBase,
    currencyPairQuote,
    newCurrencyPair,
    ExchangeRate,
    exchangeRatePair,
    exchangeRatePrice,
    newExchangeRate,
    newExchangeRate',
    tweakExchangeRate,
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

data ExchangeRate
  = ExchangeRate
      { exchangeRateAction :: ExchangeAction,
        exchangeRatePair :: CurrencyPair,
        exchangeRatePrice :: PosRat
      }
  deriving (Eq, Ord, Show)

newExchangeRate ::
  ExchangeAction ->
  Rational ->
  CurrencyCode 'Base ->
  CurrencyCode 'Quote ->
  Either Error ExchangeRate
newExchangeRate act rat base quote = do
  pair <- newCurrencyPair base quote
  newExchangeRate' act rat pair

newExchangeRate' ::
  ExchangeAction ->
  Rational ->
  CurrencyPair ->
  Either Error ExchangeRate
newExchangeRate' act rat pair = do
  posRat <- newPosRat rat
  Right $ ExchangeRate act pair posRat

tweakExchangeRate ::
  (PosRat -> PosRat) ->
  ExchangeRate ->
  ExchangeRate
tweakExchangeRate tweak rate =
  rate
    { exchangeRatePrice =
        tweak $
          exchangeRatePrice rate
    }
