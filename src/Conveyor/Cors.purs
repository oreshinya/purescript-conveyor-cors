module Conveyor.Cors
  ( Settings
  , defaultSettings
  , Cors, cors
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Conveyor.Respondable (class Respondable, toResponder)
import Conveyor.Servable (class Servable, serve)
import Conveyor.Types (Responder(..), RawData)
import Data.Array (length)
import Data.Foreign (toForeign)
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
  toResponder (StatusOnly code) =
    Responder
      { contentType: "text/plain; charset=utf-8"
      , code
      , body: toForeign ""
      }

data Cors server = Cors Settings server



defaultSettings :: Settings
defaultSettings =
  { origin: "*"
  , credentials: false
  , exposeHeaders: []
  , maxAge: Nothing
  , allowHeaders: []
  }



cors :: forall ex eff server. Servable ex eff server => Settings -> server -> Cors server
cors = Cors



setCorsHeaders
  :: forall ex eff server
   . Servable ex (http :: HTTP | eff) server
  => Settings
  -> server
  -> ex
  -> RawData
  -> Aff (http :: HTTP | eff) Responder
setCorsHeaders settings server extraData rawData = do
  liftEff $ vary rawData.res "Origin"
  liftEff $ setOrigin settings rawData.res
  if isNotPreflight rawData.req
    then do
      liftEff $ setCorsHeadersIfNotPreflight settings rawData.res
      serve server extraData rawData
    else liftEff do
      setCorsHeadersIfPreflight settings rawData.req rawData.res
      pure $ toResponder $ StatusOnly 204



setCorsHeadersIfPreflight :: forall eff. Settings -> Request -> Response -> Eff (http :: HTTP | eff) Unit
setCorsHeadersIfPreflight settings req res = do
  setCredentials settings res
  setMaxAge settings res
  setAllowMethods res
  setAllowHeaders settings req res



setCorsHeadersIfNotPreflight :: forall eff. Settings -> Response -> Eff (http :: HTTP | eff) Unit
setCorsHeadersIfNotPreflight settings res = do
  setCredentialsIfSpecifiedOrigin settings res
  setExposeHeaders settings res



setAllowHeaders :: forall eff. Settings -> Request -> Response -> Eff (http :: HTTP | eff) Unit
setAllowHeaders settings req res =
  if (length settings.allowHeaders) <= 0
    then setAllowHeadersFromRequest req res
    else setAllowHeadersFromSettings settings res



setAllowHeadersFromRequest :: forall eff. Request -> Response -> Eff (http :: HTTP | eff) Unit
setAllowHeadersFromRequest req res =
  case (requestHeader req "access-control-request-headers") of
    Nothing -> pure unit
    Just h -> setHeader res "Access-Control-Allow-Headers" h



setAllowHeadersFromSettings :: forall eff. Settings -> Response -> Eff (http :: HTTP | eff) Unit
setAllowHeadersFromSettings settings res =
  setHeaders res "Access-Control-Allow-Headers" settings.allowHeaders



setAllowMethods :: forall eff. Response -> Eff (http :: HTTP | eff) Unit
setAllowMethods res =
  setHeaders res "Access-Control-Allow-Methods" [ "OPTIONS", "POST" ]



setMaxAge :: forall eff. Settings -> Response -> Eff (http :: HTTP | eff) Unit
setMaxAge settings res =
  case settings.maxAge of
    Nothing -> pure unit
    Just i -> setHeader res "Access-Control-Max-Age" $ show i



setExposeHeaders :: forall eff. Settings -> Response -> Eff (http :: HTTP | eff) Unit
setExposeHeaders settings res =
  if (length settings.exposeHeaders) <= 0
    then pure unit
    else setHeaders res "Access-Control-Expose-Headers" settings.exposeHeaders



setCredentialsIfSpecifiedOrigin :: forall eff. Settings -> Response -> Eff (http :: HTTP | eff) Unit
setCredentialsIfSpecifiedOrigin settings res =
  if settings.origin == "*"
    then pure unit
    else setCredentials settings res



setCredentials :: forall eff. Settings -> Response -> Eff (http :: HTTP | eff) Unit
setCredentials settings res =
  if settings.credentials
    then setHeader res "Access-Control-Allow-Credentials" "true"
    else pure unit



setOrigin :: forall eff. Settings -> Response -> Eff (http :: HTTP | eff) Unit
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



instance serverableCors :: Servable ex (http :: HTTP | eff) server => Servable ex (http :: HTTP | eff) (Cors server) where
  serve (Cors settings server) extraData rawData =
    if isNoOrigin rawData.req
      then serve server extraData rawData
      else setCorsHeaders settings server extraData rawData
