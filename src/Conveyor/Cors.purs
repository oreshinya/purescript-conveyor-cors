module Conveyor.Cors
  ( Settings
  , defaultSettings
  , cors
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except (ExceptT, throwError)
import Conveyor (Break(..), Context(..))
import Data.Array (length, nub, (:))
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), split, joinWith)
import Data.StrMap (lookup, member)
import Node.HTTP (HTTP, Request, requestHeaders, setHeader, setHeaders, requestMethod)



type Settings =
  { origin :: String
  , credentials :: Boolean
  , exposeHeaders :: Array String
  , maxAge :: Maybe Int
  , allowHeaders :: Array String
  }



defaultSettings :: Settings
defaultSettings =
  { origin: "*"
  , credentials: false
  , exposeHeaders: []
  , maxAge: Nothing
  , allowHeaders: []
  }



cors :: forall e. Settings -> Context -> ExceptT Break (Eff (http :: HTTP | e)) Context
cors settings ctx =
  if isNoOrigin ctx
    then pure ctx
    else setCorsHeaders settings ctx



setCorsHeaders :: forall e. Settings -> Context -> ExceptT Break (Eff (http :: HTTP | e)) Context
setCorsHeaders settings ctx = do
  liftEff $ setOriginToVary ctx
  liftEff $ setOrigin settings ctx
  if isNotPreflight ctx
    then do
      liftEff $ setCorsHeadersIfNotPreflight settings ctx
      pure ctx
    else do
      liftEff $ setCorsHeadersIfPreflight settings ctx
      throwError $ Break { status: 204, message: Nothing }



setCorsHeadersIfPreflight :: forall e. Settings -> Context -> Eff (http :: HTTP | e) Unit
setCorsHeadersIfPreflight settings ctx = do
  setCredentials settings ctx
  setMaxAge settings ctx
  setAllowMethods ctx
  setAllowHeaders settings ctx




setCorsHeadersIfNotPreflight :: forall e. Settings -> Context -> Eff (http :: HTTP | e) Unit
setCorsHeadersIfNotPreflight settings ctx = do
  setCredentialsIfSpecifiedOrigin settings ctx
  setExposeHeaders settings ctx



setAllowHeaders :: forall e. Settings -> Context -> Eff (http :: HTTP | e) Unit
setAllowHeaders settings ctx =
  if (length settings.allowHeaders) <= 0
    then setAllowHeadersFromRequest ctx
    else setAllowHeadersFromSettings settings ctx



setAllowHeadersFromRequest :: forall e. Context -> Eff (http :: HTTP | e) Unit
setAllowHeadersFromRequest (Context { req, res }) =
  case (requestHeader req "Access-Control-Request-Headers") of
    Nothing -> pure unit
    Just h -> setHeader res "Access-Control-Allow-Headers" h



setAllowHeadersFromSettings :: forall e. Settings -> Context -> Eff (http :: HTTP | e) Unit
setAllowHeadersFromSettings settings (Context { res }) =
  setHeaders res "Access-Control-Allow-Headers" settings.allowHeaders



setAllowMethods :: forall e. Context -> Eff (http :: HTTP | e) Unit
setAllowMethods (Context { res }) =
  setHeaders res "Access-Control-Allow-Methods" [ "OPTIONS", "POST" ]



setMaxAge :: forall e. Settings -> Context -> Eff (http :: HTTP | e) Unit
setMaxAge settings (Context { res }) =
  case settings.maxAge of
    Nothing -> pure unit
    Just i -> setHeader res "Access-Control-Max-Age" $ show i



setExposeHeaders :: forall e. Settings -> Context -> Eff (http :: HTTP | e) Unit
setExposeHeaders settings (Context { res }) =
  if (length settings.exposeHeaders) <= 0
    then pure unit
    else setHeaders res "Access-Control-Expose-Headers" settings.exposeHeaders



setCredentialsIfSpecifiedOrigin :: forall e. Settings -> Context -> Eff (http :: HTTP | e) Unit
setCredentialsIfSpecifiedOrigin settings ctx =
  if settings.origin == "*"
    then pure unit
    else setCredentials settings ctx



setCredentials :: forall e. Settings -> Context -> Eff (http :: HTTP | e) Unit
setCredentials settings (Context { res }) =
  if settings.credentials
    then setHeader res "Access-Control-Allow-Credentials" "true"
    else pure unit



setOrigin :: forall e. Settings -> Context -> Eff (http :: HTTP | e) Unit
setOrigin settings (Context { res }) =
  setHeader res "Access-Control-Allow-Origin" settings.origin



setOriginToVary :: forall e. Context -> Eff (http :: HTTP | e) Unit
setOriginToVary (Context { req, res }) =
  setHeader res "Vary" $ maybe "Origin" addOriginToVary $ requestHeader req "Vary"



addOriginToVary :: String -> String
addOriginToVary vary = joinWith divider $ nub $ "Origin" : split pattern vary



requestHeader :: Request -> String -> Maybe String
requestHeader req key = lookup key $ requestHeaders req



existsRequestHeader :: Request -> String -> Boolean
existsRequestHeader req key = member key $ requestHeaders req



isNoOrigin :: Context -> Boolean
isNoOrigin (Context { req }) = not $ existsRequestHeader req "Origin"



isNotPreflight :: Context -> Boolean
isNotPreflight (Context { req }) = (requestMethod req) /= "OPTIONS"



divider :: String
divider = ","



pattern :: Pattern
pattern = Pattern divider
