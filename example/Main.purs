module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Conveyor (run)
import Conveyor.Cors (Settings, defaultSettings, cors)
import Conveyor.Handler (Handler)
import Conveyor.Respondable (class Respondable)
import Data.Foreign.Class (class Encode)
import Data.Foreign.Generic (defaultOptions, genericEncode, encodeJSON)
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Node.HTTP (HTTP, ListenOptions)
import Node.Process (PROCESS, lookupEnv)



data Result r
  = Success { status :: Int, body :: r }
  | Failure { status :: Int, message :: String }

instance respondableResult :: Encode r => Respondable (Result r) where
  statusCode (Success s) = s.status
  statusCode (Failure f) = f.status

  encodeBody (Success s) = encodeJSON s.body
  encodeBody (Failure f) = "{ \"message\": [\"" <> f.message <> "\"] }"

  systemError _ = Failure { status: 500, message: "Internal server error" }


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
myJson = pure $ Success
  { status: 200
  , body: MyJson { content: "test content :)" }
  }



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
  run (cors corsSettings { myJson }) config
