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
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module VtUtils.HTTP
    ( httpContentTypeJSON
    , httpRequestPath
    , httpRequestBodyText
    , HTTPRequestBodyJSONException(..)
    , httpRequestBodyJSON
    , httpRequestHeaders
    , httpRequestHeadersMap
    -- client
    , HTTPResponseBodyException(..)
    , httpResponseBody
    , httpResponseBodyText
    , HTTPResponseBodyJSONException(..)
    , httpResponseBodyJSON
    , httpResponseHeaders
    , httpResponseHeadersMap
    ) where

import Prelude (Either(..), Int, IO, Show(..), String, (.), ($), (>=), (<$>), fromIntegral, return)
import Control.Exception (Exception(..), throwIO)
import Control.Monad (when)
import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as ByteStringLazy
import Data.CaseInsensitive (original)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Network.HTTP.Client (BodyReader, Response, brReadSome, responseBody, responseHeaders)
import Network.HTTP.Types (Header)
import Network.Wai (Request, lazyRequestBody, rawPathInfo, requestHeaders, strictRequestBody)

import VtUtils.Error (errorShow)
import VtUtils.Text (textDecodeUtf8, textShow)

uncase :: Header -> (Text, Text)
uncase (name, val) = ((textDecodeUtf8 . original) name, (textDecodeUtf8 val))

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
httpRequestPath = textDecodeUtf8 . rawPathInfo

-- | Reads a body of the specified HTTP request as a @Text@ string
--
-- Arguments:
--
--    * @req :: Request@: HTTP request
--
-- Return value: Request body as a @Text@ string
--
httpRequestBodyText :: Request -> IO Text
httpRequestBodyText req = (textDecodeUtf8 . ByteStringLazy.toStrict) <$> strictRequestBody req

-- | Exception for `httpRequestBodyJSON` function
--
data HTTPRequestBodyJSONException = HTTPRequestBodyJSONException
    { requestBody :: ByteString -- ^ Request body containing invalid JSON
    , message :: Text -- ^ JSON parsing error message
    }
instance Exception HTTPRequestBodyJSONException
instance Show HTTPRequestBodyJSONException where
    show e@(HTTPRequestBodyJSONException {requestBody, message}) = errorShow e $
               "JSON decoding error,"
            <> " message: [" <> message <> "],"
            <> " request body: [" <> (Text.take 1024 $ textDecodeUtf8 requestBody) <> "]"

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
-- Throws an exception if request body doesn't contain valid JSON.
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
        Left err -> throwIO $ HTTPRequestBodyJSONException
                { requestBody = ByteStringLazy.toStrict bs
                , message = pack err
                }
        Right res -> return res

-- | Headers of the specified HTTP request as a @Vector@ of @(name, value)@ pairs
--
-- Arguments:
--
--    * @req :: Request@: HTTP request
--
-- Return value: Request headers as a @Vector@ of @(name, value)@ pairs
--
httpRequestHeaders :: Request -> Vector (Text, Text)
httpRequestHeaders req = Vector.fromList (uncase <$> requestHeaders req)

-- | Headers of the specified HTTP request as a @name -> value@ map
--
-- Arguments:
--
--    * @req :: Request@: HTTP request
--
-- Return value: Request headers as a @name -> value@ map
--
httpRequestHeadersMap :: Request -> HashMap Text Text
httpRequestHeadersMap req = HashMap.fromList (uncase <$> requestHeaders req)

-- | Exception for `httpRequestBodyJSON` function
--
data HTTPResponseBodyException = HTTPResponseBodyException
    { threshold :: Int -- ^ Max allowed bytes to read
    , read :: Int -- ^ Bytes actually read
    , label :: Text -- ^ Caller-supplied label
    , responsePart :: ByteString -- ^ Part of the response that was read
    }
instance Exception HTTPResponseBodyException
instance Show HTTPResponseBodyException where
    show e@(HTTPResponseBodyException {threshold, read, label, responsePart}) = errorShow e $
               "HTTP response size threshold exceeded,"
            <> " threshold: [" <> (textShow threshold) <> "],"
            <> " read: [" <> (textShow read) <> "],"
            <> " label: [" <> label <> "],"
            <> " response part: [" <> (Text.take 1024 $ textDecodeUtf8 responsePart) <> "]"

-- | Read a body of HTTP response as a lazy @ByteString@
--
-- Throws an exception if specified threshold is exceeded.
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
    let read = (ByteStringLazy.length lbs)
    when (read >= (fromIntegral threshold)) $ throwIO $ HTTPResponseBodyException
            { threshold = threshold
            , read = fromIntegral read
            , label = label
            , responsePart = ByteStringLazy.toStrict lbs
            }
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
    let tx = textDecodeUtf8 (ByteStringLazy.toStrict lbs)
    return tx

-- | Exception for `httpResponseBodyJSON` function
--
data HTTPResponseBodyJSONException = HTTPResponseBodyJSONException
    { response :: ByteString -- ^ Response body containing invalid JSON
    , label :: Text -- ^ Caller-supplied label
    , message :: Text -- ^ JSON parsing error message
    }
instance Exception HTTPResponseBodyJSONException
instance Show HTTPResponseBodyJSONException where
    show e@(HTTPResponseBodyJSONException {response, label, message}) = errorShow e $
               "JSON decoding error,"
            <> " message: [" <> message <> "],"
            <> " label: [" <> label <> "],"
            <> " response: [" <> (Text.take 1024 $ textDecodeUtf8 response) <> "]"

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
-- Throws an exception if response body doesn't contain valid JSON.
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
        Left err -> throwIO $ HTTPResponseBodyJSONException
                { message = pack err
                , label = label
                , response = ByteStringLazy.toStrict bs
                }
        Right res -> return res

-- | Headers of the specified HTTP response as a @Vector@ of @(name, value)@ pairs
--
-- Arguments:
--
--    * @req :: Response@: HTTP request
--
-- Return value: Response headers as a @Vector@ of @(name, value)@ pairs
--
httpResponseHeaders :: Response a -> Vector (Text, Text)
httpResponseHeaders resp = Vector.fromList (uncase <$> responseHeaders resp)

-- | Headers of the specified HTTP response as a @name -> value@ map
--
-- Arguments:
--
--    * @req :: Response@: HTTP request
--
-- Return value: Response headers as a @name -> value@ map
--
httpResponseHeadersMap :: Response a -> HashMap Text Text
httpResponseHeadersMap resp = HashMap.fromList (uncase <$> responseHeaders resp)
