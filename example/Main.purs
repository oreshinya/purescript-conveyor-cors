module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Conveyor (run)
import Conveyor.Cors (Settings, defaultSettings, cors)
import Conveyor.Handler (Handler)
import Conveyor.Responsable (Result, result)
import Data.Foreign.Class (class Encode)
import Data.Foreign.Generic (defaultOptions, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Node.HTTP (HTTP, ListenOptions)
import Node.Process (PROCESS, lookupEnv)



newtype MyJson = MyJson { content :: String }

derive instance genericMyJson :: Generic MyJson _

instance encodeMyJson :: Encode MyJson where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }



getHostname :: forall e. Eff e String
getHostname = pure "0.0.0.0"



getPort :: forall e. Eff (process :: PROCESS | e) Int
getPort = do
  portStr <- lookupEnv "PORT"
  pure $ case (map fromString portStr) of
    Just (Just i) -> i
    _ -> 3000



getBacklog :: forall e. Eff e (Maybe Int)
getBacklog = pure Nothing



getConfig :: forall e. Eff (process :: PROCESS | e) ListenOptions
getConfig = do
  hostname <- getHostname
  port <- getPort
  backlog <- getBacklog
  pure { hostname, port, backlog }



myJson :: forall e. Handler e (Result MyJson)
myJson = pure $ result 200 $ MyJson { content: "test content :)" }



corsSettings :: Settings
corsSettings = defaultSettings
  { origin = "http://example.com"
  , credentials = true
  , exposeHeaders = [ "X-My-Custom-Header", "X-Your-Custom-Header" ]
  , maxAge = Just 60
  , allowHeaders = [ "origin" ]
  }



main :: forall e. Eff (process :: PROCESS, exception :: EXCEPTION, ref :: REF, http :: HTTP | e ) Unit
main = do
  config <- getConfig
  run config $ cors corsSettings { myJson }
