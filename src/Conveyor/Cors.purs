module Conveyor.Cors
  ( Settings
  , defaultSettings
  , Cors, cors
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Conveyor.Argument (RawData(..))
import Conveyor.Respondable (class Respondable, Responder(..), toResponder)
import Conveyor.Servable (class Servable, serve)
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
      { contentType: "text/plain"
      , code
      , body: toForeign ""
      }
  fromError _ = StatusOnly 500

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



setCorsHeaders
  :: forall c e s
   . Servable c (http :: HTTP | e) s
  => Settings
  -> s
  -> c
  -> RawData
  -> Aff (http :: HTTP | e) Responder
setCorsHeaders settings servable ctx rawData@(RawData rd) = do
  liftEff $ vary rd.res "Origin"
  liftEff $ setOrigin settings rd.res
  if isNotPreflight rd.req
    then do
      liftEff $ setCorsHeadersIfNotPreflight settings rd.res
      serve servable ctx rawData
    else liftEff do
      setCorsHeadersIfPreflight settings rd.req rd.res
      pure $ toResponder $ StatusOnly 204



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



instance serverableCors :: Servable c (http :: HTTP | e) s => Servable c (http :: HTTP | e) (Cors s) where
  serve (Cors settings servable) ctx rawData@(RawData rd) =
    if isNoOrigin rd.req
      then serve servable ctx rawData
      else setCorsHeaders settings servable ctx rawData
