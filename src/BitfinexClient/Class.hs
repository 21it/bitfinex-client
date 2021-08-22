module BitfinexClient.Class
  ( EnvM (..),
  )
where

import BitfinexClient.Data.Env (Env (..))
import BitfinexClient.Import.External

class MonadIO m => EnvM m where
  getEnv :: m Env
