module Main where

import Prelude

import Conveyor (handler)
import Conveyor.Cors (Settings, defaultSettings, cors)
import Conveyor.Respondable (class Respondable, class RespondableError)
import Conveyor.Types (Responder(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Node.HTTP (ListenOptions, createServer, listen)
import Node.Process (lookupEnv)
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

instance respondableErrorResult :: WriteForeign r => RespondableError (Result r) where
  fromError _ = Failure { status: 500, message: "Internal server error ;)" }



getHostname :: Effect String
getHostname = pure "0.0.0.0"



getPort :: Effect Int
getPort = do
  portStr <- lookupEnv "PORT"
  pure $ case (map fromString portStr) of
    Just (Just i) -> i
    _ -> 3000



getBacklog :: Effect (Maybe Int)
getBacklog = pure Nothing



getConfig :: Effect ListenOptions
getConfig = do
  hostname <- getHostname
  port <- getPort
  backlog <- getBacklog
  pure { hostname, port, backlog }



myJson :: Aff (Result MyJson)
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



main :: Effect Unit
main = do
  config <- getConfig
  server <- createServer $ handler $ cors corsSettings { myJson }
  listen server config $ pure unit
