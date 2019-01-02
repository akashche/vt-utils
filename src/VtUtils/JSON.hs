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
-- JSON utilities
--
-- Example of the data type, that supports JSON encoding and decoding with [Aeson](https://hackage.haskell.org/package/aeson):
--
-- >   data Foo = Foo
-- >       { foo :: Int
-- >       , bar :: Text
-- >       } deriving Generic
-- >   instance FromJSON Foo
-- >   instance ToJSON Foo
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module VtUtils.JSON
    ( jsonDecodeFile
    , jsonDecodeText
    , jsonEncodeText
    , jsonGet
    ) where

import Prelude (Either(..), IO, String, (.), error, return)
import Data.Aeson (FromJSON, Value(..), ToJSON, eitherDecode)
import Data.Aeson.Encode.Pretty (encodePrettyToTextBuilder)
import Data.Aeson.Types ((.:), parseEither)
import Data.ByteString.Lazy (fromChunks)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)

import VtUtils.IO

-- | Encodes a data into a JSON @Text@ string
--
-- Data must be an instance of [ToJSON](https://hackage.haskell.org/package/aeson-1.4.2.0/docs/Data-Aeson.html#t:ToJSON)
--
-- Throws an error if data cannot be encoded
--
-- Arguments:
--
--    * @data :: ToJSON@: some data that supports JSON serialization with Aeson
--
-- Return value: JSON @Text@ string
--
jsonEncodeText :: ToJSON a => a -> Text
jsonEncodeText = toStrict . toLazyText . encodePrettyToTextBuilder

-- | Parses a JSON @Text@ string into a typed data
--
-- Data type should be specified with a type annotation:
--
-- Example:
--
-- >
-- > let dt = jsonDecodeText text :: Foo
-- >
--
-- Data must be an instance of [FromJSON](https://hackage.haskell.org/package/aeson-1.4.2.0/docs/Data-Aeson.html#t:FromJSON)
--
-- Throws an error if data cannot be decoded
--
-- Arguments:
--
--    * @text :: Text@: JSON @Text@ string to parse
--
-- Return value: Decoded data
--
jsonDecodeText :: forall a . FromJSON a => Text -> a
jsonDecodeText text =
    case eitherDecode bs :: Either String a of
        Left err -> (error . unpack)
            (  "Error decoding JSON,"
            <> " message: [" <> pack err <> "]")
        Right res -> res
    where
        bs = fromChunks [encodeUtf8 text]

-- | Parses contents of a specified JSON file into a typed data
--
-- Data type should be specified with a type annotation:
--
-- Example:
--
-- >
-- > dt <- jsonDecodeFile "path/to/foo.json" :: IO Foo
-- >
--
-- Data must be an instance of [FromJSON](https://hackage.haskell.org/package/aeson-1.4.2.0/docs/Data-Aeson.html#t:FromJSON)
--
-- File contents are decoded as @UTF-8@
--
-- Throws an error if file cannot be read or data cannot be decoded
--
-- Arguments:
--
--    * @path :: Text@: Path to JSON file
--
-- Return value: Decoded data
--
jsonDecodeFile :: forall a . FromJSON a => Text -> IO a
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

-- | Extract the field value from the specified JSON object
--
-- Throws an error, if specified JSON @Value@ is not a JSON object,
-- if it does't contain a specified field, if field type is different
-- from the one specified in type annotation
--
-- Data type should be specified with a type annotation:
--
-- >
-- > let obj = object
-- >         [ "foo" .= (42 :: Int)
-- >         , "bar" .= ("baz" :: Text)
-- >         ]
-- > let fooval = jsonGet obj "foo" :: Int
-- > let barval = jsonGet obj "bar" :: Text
-- >
--
-- Arguments:
--
--    * @val :: Aeson.Value@: JSON value, must be a JSON object
--    * @field :: Text@: Field name
--
-- Return value: Field value
--
jsonGet :: forall a . FromJSON a => Value -> Text -> a
jsonGet val field =
    case val of
        Object obj -> case parseEither (.: field) obj :: Either String a of
            Left err -> (error . unpack)
                 (  "Error accessing field,"
                 <> " name: [" <> field <> "],"
                 <> " object: [" <> (jsonEncodeText obj) <> "]"
                 <> " message: [" <> (pack err) <> "]")
            Right a -> a
        _ -> (error .unpack)
                ( "Invalid non-object JSON value specified,"
                <> " value: [" <> (jsonEncodeText val) <> "]")
