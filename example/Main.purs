module Main where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Conveyor (handler)
import Conveyor.Cors (Settings, defaultSettings, cors)
import Conveyor.Respondable (class Respondable, Responder(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Node.HTTP (HTTP, ListenOptions, createServer, listen)
import Node.Process (PROCESS, lookupEnv)
import Simple.JSON (class WriteForeign, write)



data Result r
  = Success { status :: Int, body :: r }
  | Failure { status :: Int, message :: String }

type MyJson = { content :: String }



instance respondableResult :: WriteForeign r => Respondable (Result r) where
  toResponder (Success s) =
    Responder
      { contentType: "application/json; charset=utf-8"
      , code: s.status
      , body: write s.body
      }
  toResponder (Failure f) =
    Responder
      { contentType: "application/json; charset=utf-8"
      , code: f.status
      , body: write { messages: [ f.message ] }
      }
  fromError _ = Failure { status: 500, message: "Internal server error ;)" }



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



myJson :: forall e. Aff e (Result MyJson)
myJson = pure $ Success
  { status: 200
  , body: { content: "test content :)" }
  }



corsSettings :: Settings
corsSettings = defaultSettings
  { origin = "http://example.com"
  , credentials = true
  , exposeHeaders = [ "X-My-Custom-Header", "X-Your-Custom-Header" ]
  , maxAge = Just 60
  , allowHeaders = [ "origin" ]
  }



main :: forall e. Eff (process :: PROCESS, http :: HTTP | e ) Unit
main = do
  config <- getConfig
  server <- createServer $ handler $ cors corsSettings { myJson }
  listen server config $ pure unit
