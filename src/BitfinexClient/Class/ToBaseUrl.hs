{-# LANGUAGE AllowAmbiguousTypes #-}

module BitfinexClient.Class.ToBaseUrl
  ( ToBaseUrl (..),
  )
where

import BitfinexClient.Data.Kind
import BitfinexClient.Data.Type

class ToBaseUrl (method :: Method) where
  toBaseUrl :: BaseUrl

instance ToBaseUrl 'MarketAveragePrice where
  toBaseUrl = prv

instance ToBaseUrl 'FeeSummary where
  toBaseUrl = prv

instance ToBaseUrl 'SubmitOrder where
  toBaseUrl = prv

instance ToBaseUrl 'RetrieveOrders where
  toBaseUrl = prv

instance ToBaseUrl 'OrdersHistory where
  toBaseUrl = prv

prv :: BaseUrl
prv = "https://api.bitfinex.com"
