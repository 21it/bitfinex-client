module BitfinexClient.Class.ToQueryParam
  ( ToQueryParam (..),
    SomeQueryParam (..),
    unQueryParam,
  )
where

import BitfinexClient.Import.External
import qualified Data.ByteString as BS
import qualified Data.Text as T

--
-- TODO : rename ToRequestParam
--
class ToQueryParam a where
  toBodyParam :: a -> Text
  toQueryParam :: a -> Maybe BS.ByteString
  toQueryParam = Just . encodeUtf8 . toBodyParam

data SomeQueryParam
  = forall a. ToQueryParam a => SomeQueryParam BS.ByteString a

instance ToQueryParam Rational where
  toBodyParam x =
    T.pack $ showFixed True (fromRational x :: Fixed E12)

unQueryParam :: SomeQueryParam -> (BS.ByteString, Maybe BS.ByteString)
unQueryParam (SomeQueryParam name x) = (name, toQueryParam x)
