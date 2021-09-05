module BitfinexClient.Data.Money
  ( CurrencyCode (..),
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

import BitfinexClient.Class.ToQueryParam
import BitfinexClient.Data.Kind
import BitfinexClient.Data.Type
import BitfinexClient.Import.External

newtype CurrencyCode (a :: CurrencyRelation)
  = CurrencyCode Text
  deriving newtype (Eq, Ord, Show, IsString)

data CurrencyPair
  = CurrencyPair
      { currencyPairBase :: CurrencyCode 'Base,
        currencyPairQuote :: CurrencyCode 'Quote
      }
  deriving (Eq, Ord, Show)

instance ToQueryParam CurrencyPair where
  toBodyParam x =
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
      { exchangeRatePair :: CurrencyPair,
        exchangeRatePrice :: PosRat
      }
  deriving (Eq, Ord, Show)

newExchangeRate ::
  CurrencyCode 'Base ->
  CurrencyCode 'Quote ->
  Rational ->
  Either Error ExchangeRate
newExchangeRate base quote rat = do
  pair <- newCurrencyPair base quote
  newExchangeRate' pair rat

newExchangeRate' ::
  CurrencyPair ->
  Rational ->
  Either Error ExchangeRate
newExchangeRate' pair rat = do
  posRat <- newPosRat rat
  Right $ ExchangeRate pair posRat

tweakExchangeRate :: (PosRat -> PosRat) -> ExchangeRate -> ExchangeRate
tweakExchangeRate tweak rate =
  rate
    { exchangeRatePrice =
        tweak $
          exchangeRatePrice rate
    }
