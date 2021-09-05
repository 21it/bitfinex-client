module BitfinexClient.Class.ToRequestParam
  ( ToRequestParam (..),
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
class ToRequestParam a where
  toBodyParam :: a -> Text
  toQueryParam :: a -> Maybe BS.ByteString
  toQueryParam = Just . encodeUtf8 . toBodyParam

data SomeQueryParam
  = forall a. ToRequestParam a => SomeQueryParam BS.ByteString a

instance ToRequestParam Rational where
  toBodyParam x =
    T.pack $ showFixed True (fromRational x :: Fixed E12)

unQueryParam :: SomeQueryParam -> (BS.ByteString, Maybe BS.ByteString)
unQueryParam (SomeQueryParam name x) = (name, toQueryParam x)
