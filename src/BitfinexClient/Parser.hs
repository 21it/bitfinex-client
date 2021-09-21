{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Parser
  ( parseOrder,
    parseOrderMap,
  )
where

import BitfinexClient.Data.Type
import BitfinexClient.Import.External
import Data.Aeson.Lens
import qualified Data.Map as Map

parseOrder :: (AsValue a, Show a) => a -> Either Error (Order b)
parseOrder x = do
  id0 <-
    maybeToRight (failure "OrderId is missing") $
      OrderId
        <$> x ^? nth 0 . _Integral
  gid <-
    maybeToRight (failure "OrderGroupId is missing") $
      (Just . OrderGroupId <$> x ^? nth 1 . _Integral)
        <|> (Nothing <$ x ^? nth 1 . _Null)
  cid <-
    maybeToRight (failure "OrderClientId is missing") $
      (Just . OrderClientId <$> x ^? nth 2 . _Integral)
        <|> (Nothing <$ x ^? nth 2 . _Null)
  sym0 <-
    maybeToRight (failure "Symbol is missing") $
      x ^? nth 3 . _String
  sym <-
    newCurrencyPair' sym0
  amt0 <-
    maybeToRight (failure "OrderAmount is missing") $
      toRational <$> x ^? nth 7 . _Number
  amt <-
    newMoneyAmount $ abs amt0
  act <-
    newExchangeAction amt0
  ss0 <-
    maybeToRight (failure "OrderStatus is missing") $
      x ^? nth 13 . _String
  ss1 <-
    first failure $
      newOrderStatus ss0
  price <-
    maybeToRight
      (failure "ExchangeRate is missing")
      $ x ^? nth 16 . _Number
  rate <-
    newExchangeRate $ toRational price
  pure
    Order
      { orderId = id0,
        orderGroupId = gid,
        orderClientId = cid,
        orderAction = act,
        orderAmount = amt,
        orderSymbol = sym,
        orderRate = rate,
        orderStatus = ss1
      }
  where
    failure =
      ErrorFromRpc . (<> " in " <> show x)

parseOrderMap :: AsValue a => a -> Either Error (Map OrderId (Order b))
parseOrderMap raw = do
  xs <-
    maybeToRight
      (ErrorFromRpc "Json is not an Array")
      $ raw ^? _Array
  foldrM parser mempty xs
  where
    parser x acc = do
      order <- parseOrder x
      pure $ Map.insert (orderId order) order acc
