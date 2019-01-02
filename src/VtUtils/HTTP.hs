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
-- HTTP utilities
-- TODO
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
    ) where

import Prelude (Either(..), IO, String, (.), (<$>), fst, error, return, snd)
import Data.Aeson (FromJSON, eitherDecode)
import Data.CaseInsensitive (original)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Network.HTTP.Types (Header)
import Network.Wai (Request, lazyRequestBody, rawPathInfo, requestBody, requestHeaders)

httpContentTypeJSON :: Header
httpContentTypeJSON = ("Content-Type", "application/json")

httpRequestPath :: Request -> Text
httpRequestPath = decodeUtf8 . rawPathInfo

httpRequestBodyText :: Request -> IO Text
httpRequestBodyText req = decodeUtf8 <$> requestBody req

httpRequestBodyJSON :: forall a . FromJSON a => Request -> IO a
httpRequestBodyJSON req = do
    bs <- lazyRequestBody req
    case eitherDecode bs :: Either String a of
        Left err -> (error . unpack)
            (  "Error decoding JSON,"
            <> " message: [" <> pack err <> "]")
        Right res -> return res

httpRequestHeadersList :: Request -> [(Text, Text)]
httpRequestHeadersList req =
    uncase <$> requestHeaders req
    where
        decodeFst = decodeUtf8 . original . fst
        decodeSnd = decodeUtf8 . snd
        uncase el = (decodeFst el, decodeSnd el)

httpRequestHeaders :: Request -> Vector (Text, Text)
httpRequestHeaders = Vector.fromList . httpRequestHeadersList

httpRequestHeadersMap :: Request -> HashMap Text Text
httpRequestHeadersMap = HashMap.fromList . httpRequestHeadersList
