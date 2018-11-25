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

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module VtUtils.Json
    ( jsonDecodeFile
    , jsonDecodeText
    , jsonEncodeText
    , jsonGet
    ) where

import Prelude (Either(..), IO, String, (.), error, return)
import Data.Aeson (FromJSON, Value(..), ToJSON, eitherDecode, encode)
import Data.Aeson.Types ((.:), parseEither)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteStringLazy

import VtUtils.IO

jsonEncodeText :: ToJSON a => a -> Text
jsonEncodeText = decodeUtf8 . ByteString.concat . ByteStringLazy.toChunks . encode

jsonDecodeText :: forall a . (FromJSON a) => Text -> a
jsonDecodeText tx =
    case eitherDecode bs :: Either String a of
        Left err -> (error . unpack)
            (  "Error decoding JSON,"
            <> " message: [" <> pack err <> "]")
        Right res -> res
    where
        bs = ByteStringLazy.fromChunks [encodeUtf8 tx]

jsonDecodeFile :: forall a . (FromJSON a) => Text -> IO a
jsonDecodeFile path =
    ioWithFileBytes path fun
    where
        fun bs =
            case eitherDecode bs :: Either String a of
                Left err -> (error . unpack)
                    (  "Error decoding JSON,"
                    <> " path: [" <> path <> "]"
                    <> " message: [" <> pack err <> "]")
                Right res -> return res

jsonGet :: forall a . (FromJSON a) => Value -> Text -> a
jsonGet val fieldName =
    case val of
        Object obj -> case parseEither (.: fieldName) obj :: Either String a of
            Left err -> (error . unpack)
                 (  "Error accessing field,"
                 <> " name: [" <> fieldName <> "],"
                 <> " object: [" <> (jsonEncodeText obj) <> "]"
                 <> " message: [" <> (pack err) <> "]")
            Right a -> a
        _ -> (error .unpack)
                ( "Invalid non-object JSON value specified,"
                <> " value: [" <> (jsonEncodeText val) <> "]")
