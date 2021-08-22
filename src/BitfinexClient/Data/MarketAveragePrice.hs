module BitfinexClient.Data.MarketAveragePrice
  ( Request (..),
  )
where

import BitfinexClient.Import
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Text as T
import qualified Data.Vector as V

data Request
  = Request
      { symbol :: CurrencyPair,
        amount :: Rational
      }
  deriving (Eq, Ord, Show)

instance FromRpc 'MarketAveragePrice Request SomeExchangeRate where
  fromRpc Rpc req raw = do
    obj <- fstError $ A.eitherDecode raw
    price <- fstError $ A.parseEither parser obj
    first ErrorFromRpc
      . maybeToRight "MarketAveragePrice WrongExchangeRate"
      $ mkSomeExchangeRate
        (coerce $ currencyPairBase currencyPair)
        (coerce $ currencyPairQuote currencyPair)
        (toRational price)
    where
      currencyPair = symbol req
      parser =
        A.withArray "MarketAveragePrice" $
          \xs ->
            case xs V.!? 0 of
              Nothing ->
                fail "MarketAveragePrice is missing"
              Just x ->
                A.withScientific "MarketAveragePrice" pure x
      fstError =
        first $ ErrorFromRpc . T.pack
