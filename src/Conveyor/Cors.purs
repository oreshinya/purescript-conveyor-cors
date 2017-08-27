module Conveyor.Cors
  ( Settings
  , defaultSettings
  , Cors, cors
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Conveyor.Respondable (class Respondable, ConveyorError(..), respond)
import Conveyor.Servable (class Servable, serve)
import Data.Array (length)
import Data.Maybe (Maybe(..))
import Data.StrMap (lookup, member)
import Node.HTTP (HTTP, Request, Response, requestHeaders, setHeader, setHeaders, requestMethod)
import Node.HTTP.Vary (vary)



type Settings =
  { origin :: String
  , credentials :: Boolean
  , exposeHeaders :: Array String
  , maxAge :: Maybe Int
  , allowHeaders :: Array String
  }

newtype StatusOnly = StatusOnly Int

instance respondableStatusOnly :: Respondable StatusOnly where
  statusCode (StatusOnly status) = status
  encodeBody _ = ""
  systemError _ = StatusOnly 500

data Cors s = Cors Settings s



defaultSettings :: Settings
defaultSettings =
  { origin: "*"
  , credentials: false
  , exposeHeaders: []
  , maxAge: Nothing
  , allowHeaders: []
  }



cors :: forall c e s. Servable c e s => Settings -> s -> Cors s
cors settings server = Cors settings server



setCorsHeaders :: forall c e s.
                  Servable c e s =>
                  Settings ->
                  c ->
                  s ->
                  Request ->
                  Response ->
                  String ->
                  Maybe (Eff (http :: HTTP | e) Unit)
setCorsHeaders settings ctx handler req res path = Just do
  vary res "Origin"
  setOrigin settings res
  if isNotPreflight req
    then do
      setCorsHeadersIfNotPreflight settings res
      case serve ctx handler req res path of
        Nothing -> respond res $ ConveyorError 404 "No such route"
        Just s -> s
    else do
      setCorsHeadersIfPreflight settings req res
      respond res $ StatusOnly 204



setCorsHeadersIfPreflight :: forall e. Settings -> Request -> Response -> Eff (http :: HTTP | e) Unit
setCorsHeadersIfPreflight settings req res = do
  setCredentials settings res
  setMaxAge settings res
  setAllowMethods res
  setAllowHeaders settings req res



setCorsHeadersIfNotPreflight :: forall e. Settings -> Response -> Eff (http :: HTTP | e) Unit
setCorsHeadersIfNotPreflight settings res = do
  setCredentialsIfSpecifiedOrigin settings res
  setExposeHeaders settings res



setAllowHeaders :: forall e. Settings -> Request -> Response -> Eff (http :: HTTP | e) Unit
setAllowHeaders settings req res =
  if (length settings.allowHeaders) <= 0
    then setAllowHeadersFromRequest req res
    else setAllowHeadersFromSettings settings res



setAllowHeadersFromRequest :: forall e. Request -> Response -> Eff (http :: HTTP | e) Unit
setAllowHeadersFromRequest req res =
  case (requestHeader req "access-control-request-headers") of
    Nothing -> pure unit
    Just h -> setHeader res "Access-Control-Allow-Headers" h



setAllowHeadersFromSettings :: forall e. Settings -> Response -> Eff (http :: HTTP | e) Unit
setAllowHeadersFromSettings settings res =
  setHeaders res "Access-Control-Allow-Headers" settings.allowHeaders



setAllowMethods :: forall e. Response -> Eff (http :: HTTP | e) Unit
setAllowMethods res =
  setHeaders res "Access-Control-Allow-Methods" [ "OPTIONS", "POST" ]



setMaxAge :: forall e. Settings -> Response -> Eff (http :: HTTP | e) Unit
setMaxAge settings res =
  case settings.maxAge of
    Nothing -> pure unit
    Just i -> setHeader res "Access-Control-Max-Age" $ show i



setExposeHeaders :: forall e. Settings -> Response -> Eff (http :: HTTP | e) Unit
setExposeHeaders settings res =
  if (length settings.exposeHeaders) <= 0
    then pure unit
    else setHeaders res "Access-Control-Expose-Headers" settings.exposeHeaders



setCredentialsIfSpecifiedOrigin :: forall e. Settings -> Response -> Eff (http :: HTTP | e) Unit
setCredentialsIfSpecifiedOrigin settings res =
  if settings.origin == "*"
    then pure unit
    else setCredentials settings res



setCredentials :: forall e. Settings -> Response -> Eff (http :: HTTP | e) Unit
setCredentials settings res =
  if settings.credentials
    then setHeader res "Access-Control-Allow-Credentials" "true"
    else pure unit



setOrigin :: forall e. Settings -> Response -> Eff (http :: HTTP | e) Unit
setOrigin settings res =
  setHeader res "Access-Control-Allow-Origin" settings.origin



requestHeader :: Request -> String -> Maybe String
requestHeader req key = lookup key $ requestHeaders req



existsRequestHeader :: Request -> String -> Boolean
existsRequestHeader req key = member key $ requestHeaders req



isNoOrigin :: Request -> Boolean
isNoOrigin req = not $ existsRequestHeader req "origin"



isNotPreflight :: Request -> Boolean
isNotPreflight req = (requestMethod req) /= "OPTIONS"



instance serverableCors :: Servable c e s => Servable c e (Cors s) where
  serve ctx (Cors settings handler) req res path =
    if isNoOrigin req
      then serve ctx handler req res path
      else setCorsHeaders settings ctx handler req res path
