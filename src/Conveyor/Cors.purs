module Conveyor.Cors
  ( Settings
  , defaultSettings
  , Cors, cors
  ) where

import Prelude

import Conveyor.Respondable (class Respondable, toResponder)
import Conveyor.Servable (class Servable, serve)
import Conveyor.Types (Responder(..), RawData)
import Data.Array (length)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (unsafeToForeign)
import Foreign.Object (lookup, member)
import Node.HTTP (Request, Response, requestHeaders, setHeader, setHeaders, requestMethod)
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
      , body: unsafeToForeign ""
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



cors :: forall ex server. Servable ex server => Settings -> server -> Cors server
cors = Cors



setCorsHeaders
  :: forall ex server
   . Servable ex server
  => Settings
  -> server
  -> ex
  -> RawData
  -> Aff Responder
setCorsHeaders settings server extraData rawData = do
  liftEffect $ vary rawData.res "Origin"
  liftEffect $ setOrigin settings rawData.res
  if isNotPreflight rawData.req
    then do
      liftEffect $ setCorsHeadersIfNotPreflight settings rawData.res
      serve server extraData rawData
    else liftEffect do
      setCorsHeadersIfPreflight settings rawData.req rawData.res
      pure $ toResponder $ StatusOnly 204



setCorsHeadersIfPreflight :: Settings -> Request -> Response -> Effect Unit
setCorsHeadersIfPreflight settings req res = do
  setCredentials settings res
  setMaxAge settings res
  setAllowMethods res
  setAllowHeaders settings req res



setCorsHeadersIfNotPreflight :: Settings -> Response -> Effect Unit
setCorsHeadersIfNotPreflight settings res = do
  setCredentialsIfSpecifiedOrigin settings res
  setExposeHeaders settings res



setAllowHeaders :: Settings -> Request -> Response -> Effect Unit
setAllowHeaders settings req res =
  if (length settings.allowHeaders) <= 0
    then setAllowHeadersFromRequest req res
    else setAllowHeadersFromSettings settings res



setAllowHeadersFromRequest :: Request -> Response -> Effect Unit
setAllowHeadersFromRequest req res =
  case (requestHeader req "access-control-request-headers") of
    Nothing -> pure unit
    Just h -> setHeader res "Access-Control-Allow-Headers" h



setAllowHeadersFromSettings :: Settings -> Response -> Effect Unit
setAllowHeadersFromSettings settings res =
  setHeaders res "Access-Control-Allow-Headers" settings.allowHeaders



setAllowMethods :: Response -> Effect Unit
setAllowMethods res =
  setHeaders res "Access-Control-Allow-Methods" [ "OPTIONS", "POST" ]



setMaxAge :: Settings -> Response -> Effect Unit
setMaxAge settings res =
  case settings.maxAge of
    Nothing -> pure unit
    Just i -> setHeader res "Access-Control-Max-Age" $ show i



setExposeHeaders :: Settings -> Response -> Effect Unit
setExposeHeaders settings res =
  if (length settings.exposeHeaders) <= 0
    then pure unit
    else setHeaders res "Access-Control-Expose-Headers" settings.exposeHeaders



setCredentialsIfSpecifiedOrigin :: Settings -> Response -> Effect Unit
setCredentialsIfSpecifiedOrigin settings res =
  if settings.origin == "*"
    then pure unit
    else setCredentials settings res



setCredentials :: Settings -> Response -> Effect Unit
setCredentials settings res =
  if settings.credentials
    then setHeader res "Access-Control-Allow-Credentials" "true"
    else pure unit



setOrigin :: Settings -> Response -> Effect Unit
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



instance serverableCors :: Servable ex server => Servable ex (Cors server) where
  serve (Cors settings server) extraData rawData =
    if isNoOrigin rawData.req
      then serve server extraData rawData
      else setCorsHeaders settings server extraData rawData
