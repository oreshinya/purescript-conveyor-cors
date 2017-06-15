module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except (ExceptT)
import Data.Foreign.Class (class Encode)
import Data.Foreign.Generic (defaultOptions, genericEncode)
import Data.Maybe (Maybe(..))
import Data.Int (fromString)
import Node.HTTP (HTTP, requestHeaders)
import Node.Process (PROCESS, lookupEnv)
import Data.Generic.Rep (class Generic)
import Conveyor (App(..), Config(..), Context(..), Break, Result(..), Router(..), (:>), run)
import Conveyor.Cors (Settings, defaultSettings, cors)



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



getConfig :: forall e. Eff (process :: PROCESS | e) Config
getConfig = do
  hostname <- getHostname
  port <- getPort
  backlog <- getBacklog
  pure $ Config { hostname, port, backlog }



myJson :: forall e. Context -> ExceptT Break (Eff e) (Result MyJson)
myJson _ = pure $ Result { status: 200, body: Just $ MyJson { content: "test content :)" } }



router :: forall e. Router Context e MyJson
router = Router [ "/myJson" :> myJson ]



corsSettings :: Settings
corsSettings = defaultSettings
  { origin = "http://example.com"
  , credentials = true
  , exposeHeaders = [ "X-My-Custom-Header", "X-Your-Custom-Header" ]
  , maxAge = Just 60
  , allowHeaders = [ "origin" ]
  }



respond :: forall e.
        Context ->
        (Context -> ExceptT Break (Eff (http :: HTTP, console :: CONSOLE | e)) (Result MyJson)) ->
        ExceptT Break (Eff (http :: HTTP, console :: CONSOLE | e)) (Result MyJson)
respond ctx@(Context { req }) exec = do
  liftEff $ log $ show $ requestHeaders req
  void $ cors corsSettings ctx
  exec ctx



app :: forall e. App Context (http :: HTTP, console :: CONSOLE | e) MyJson
app = App
  { router
  , respond
  }



main :: forall e. Eff (process :: PROCESS, console :: CONSOLE, exception :: EXCEPTION, ref :: REF, http :: HTTP | e ) Unit
main = do
  config <- getConfig
  run config app
