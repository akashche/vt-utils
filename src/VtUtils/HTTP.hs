--
-- Copyright 2018, akashche at redhat.com
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
-- http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
--
-- |
-- HTTP utilities for server (WAI) and client
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module VtUtils.HTTP
    ( httpContentTypeJSON
    , httpRequestPath
    , httpRequestBodyText
    , httpRequestBodyJSON
    , httpRequestHeaders
    , httpRequestHeadersMap
    -- client
    , httpResponseBody
    , httpResponseBodyText
    , httpResponseBodyJSON
    ) where

import Prelude (Either(..), Int, IO, String, (+), (.), (>), ($), (||), (<$>), fst, fromIntegral, error, return, snd)
import Control.Monad (when)
import Data.Aeson (FromJSON, eitherDecode)
import Data.CaseInsensitive (original)
import Data.HashMap.Strict (HashMap)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Vector (Vector)
import Network.HTTP.Client (BodyReader, Response, brRead, brReadSome, responseBody)
import Network.HTTP.Types (Header)
import Network.Wai (Request, lazyRequestBody, rawPathInfo, requestHeaders, strictRequestBody)
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.ByteString as ByteString
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector

import VtUtils.Text

-- | @Content-Type@ header for @application/json@ type
--
httpContentTypeJSON :: Header
httpContentTypeJSON = ("Content-Type", "application/json")

-- | URL path string of the specified HTTP request
--
-- Arguments:
--
--    * @req :: Request@: HTTP request
--
-- Return value: URL path string
--
httpRequestPath :: Request -> Text
httpRequestPath = decodeUtf8 . rawPathInfo

-- | Reads a body of the specified HTTP request as a @Text@ string
--
-- Arguments:
--
--    * @req :: Request@: HTTP request
--
-- Return value: Request body as a @Text@ string
--
httpRequestBodyText :: Request -> IO Text
httpRequestBodyText req = (decodeUtf8 . ByteStringLazy.toStrict) <$> strictRequestBody req

-- | Reads a body of the specified HTTP request and parses it as a JSON value
--
-- Data type should be specified with a type annotation:
--
-- Example:
--
-- >
-- > dt <- httpRequestBodyJSON req :: IO Foo
-- >
--
-- Data must be an instance of [FromJSON](https://hackage.haskell.org/package/aeson-1.4.2.0/docs/Data-Aeson.html#t:FromJSON)
--
-- Arguments:
--
--    * @req :: Request@: HTTP request
--
-- Return value: Request body parsed as a JSON value
--
httpRequestBodyJSON :: forall a . FromJSON a => Request -> IO a
httpRequestBodyJSON req = do
    bs <- lazyRequestBody req
    case eitherDecode bs :: Either String a of
        Left err -> error . unpack $
               "JSON decoding error,"
            <> " message: [" <> pack err <> "]"
        Right res -> return res

requestHeadersList :: Request -> [(Text, Text)]
requestHeadersList req =
    uncase <$> requestHeaders req
    where
        decodeFst = decodeUtf8 . original . fst
        decodeSnd = decodeUtf8 . snd
        uncase el = (decodeFst el, decodeSnd el)

-- | Headers of the specified HTTP request as a @Vector@ of @(name, value)@ pairs
--
-- Arguments:
--
--    * @req :: Request@: HTTP request
--
-- Return value: Request headers as a @Vector@ of @(name, value)@ pairs
--
httpRequestHeaders :: Request -> Vector (Text, Text)
httpRequestHeaders = Vector.fromList . requestHeadersList

-- | Headers of the specified HTTP request as a @name -> value@ map
--
-- Arguments:
--
--    * @req :: Request@: HTTP request
--
-- Return value: Request headers as a @name -> value@ map
--
httpRequestHeadersMap :: Request -> HashMap Text Text
httpRequestHeadersMap = HashMap.fromList . requestHeadersList

-- | Read a body of HTTP response as a lazy @ByteString@
--
-- Arguments:
--
--    * @label :: Text@: Label used for error reporting on overly-large responses
--    * @resp :: Response BodyReader@: HTTP response
--    * @threshold :: Int@ Max response body length in bytes
--
-- Return value: Response body as a lazy @ByteString@
--
httpResponseBody :: Text -> Response BodyReader -> Int -> IO ByteStringLazy.ByteString
httpResponseBody label resp threshold = do
    let reader = responseBody resp
    lbs <- brReadSome reader threshold
    rem <- ByteString.length <$> brRead reader
    let read = (ByteStringLazy.length lbs) + (fromIntegral rem)
    when (rem > 0 || read > (fromIntegral threshold)) $ error . unpack $
           "HTTP response size threshold exceeded,"
        <> " threshold: [" <> (textShow threshold) <> "],"
        <> " read: [" <> (textShow read) <> "],"
        <> " label: [" <> label <> "]"
    return lbs

-- | Read a body of HTTP response as a @Text@ string
--
-- Arguments:
--
--    * @label :: Text@: Label used for error reporting on overly-large responses
--    * @resp :: Response BodyReader@: HTTP response
--    * @threshold :: Int@ Max response body length in bytes
--
-- Return value: Response body as a @Text@ string
--
httpResponseBodyText :: Text -> Response BodyReader -> Int -> IO Text
httpResponseBodyText label resp threshold = do
    lbs <- httpResponseBody label resp threshold
    let tx = decodeUtf8 (ByteStringLazy.toStrict lbs)
    return tx

-- | Read a body of HTTP response as a JSON value
--
-- Data type should be specified with a type annotation:
--
-- Example:
--
-- >
-- > dt <- httpResponseBodyJSON label resp 1024 :: IO Foo
-- >
--
-- Data must be an instance of [FromJSON](https://hackage.haskell.org/package/aeson-1.4.2.0/docs/Data-Aeson.html#t:FromJSON)
--
-- Arguments:
--
--    * @label :: Text@: Label used for error reporting on overly-large responses
--    * @resp :: Response BodyReader@: HTTP response
--    * @threshold :: Int@ Max response body length in bytes
--
-- Return value: Response body as a JSON value
--
httpResponseBodyJSON :: forall a . FromJSON a => Text -> Response BodyReader -> Int -> IO a
httpResponseBodyJSON label resp threshold = do
    bs <- httpResponseBody label resp threshold
    case eitherDecode bs :: Either String a of
        Left err -> error . unpack $
               "JSON decoding error,"
            <> " json: [" <> ((decodeUtf8 . ByteStringLazy.toStrict) (ByteStringLazy.take 1024 bs)) <> "],"
            <> " message: [" <> pack err <> "],"
            <> " label: [" <> label <> "]"
        Right res -> return res
