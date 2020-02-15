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
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module VtUtils.JSON
    ( JSONDecodeFileException(..)
    , jsonDecodeFile
    , JSONDecodeError(..)
    , jsonDecodeText
    , JSONDecodeTextIOException(..)
    , jsonEncodeText
    , JSONGetError(..)
    , jsonGet
    , jsonUnwrapUnaryOptions
    ) where

import Prelude (Bool(..), Either(..), IO, Show, String, (.), ($), return, show)
import Control.Exception (Exception, throwIO)
import Data.Aeson (FromJSON, Value(..), ToJSON, defaultOptions, eitherDecode', eitherDecodeStrict')
import Data.Aeson.Encode.Pretty (encodePrettyToTextBuilder)
import Data.Aeson.Types (Options(..), (.:), parseEither)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)

import VtUtils.Error (errorShow)
import VtUtils.IO (ioWithFileBytes)
import VtUtils.Text (textShow)

-- | Encodes a data into a JSON @Text@ string
--
-- Data must be an instance of [ToJSON](https://hackage.haskell.org/package/aeson-1.4.2.0/docs/Data-Aeson.html#t:ToJSON)
--
-- Arguments:
--
--    * @data :: ToJSON@: some data that supports JSON serialization with Aeson
--
-- Return value: JSON @Text@ string
--
jsonEncodeText :: ToJSON a => a -> Text
jsonEncodeText = toStrict . toLazyText . encodePrettyToTextBuilder

-- | Error for `jsonDecodeText` function
--
data JSONDecodeError = JSONDecodeError
    { jsonText :: Text -- ^ Specified text
    , message :: Text -- ^ Decoding error
    }
instance Show JSONDecodeError where
    show e@(JSONDecodeError {jsonText, message}) = errorShow e $
               "Error decoding JSON,"
            <> " text: [" <> (Text.take 1024 jsonText) <> "],"
            <> " error: [" <> message <> "]"

-- | Parses a JSON @Text@ string into a typed data
--
-- Data type should be specified with a type annotation:
--
-- Example:
--
-- >
-- > let Right (dt :: Foo) = jsonDecodeText text
-- >
--
-- Data must be an instance of [FromJSON](https://hackage.haskell.org/package/aeson-1.4.2.0/docs/Data-Aeson.html#t:FromJSON)
--
-- Returns an error if data cannot be decoded
--
-- Arguments:
--
--    * @text :: Text@: JSON @Text@ string to parse
--
-- Return value: Decoded data or decoding error
--
jsonDecodeText :: forall a . FromJSON a => Text -> Either JSONDecodeError a
jsonDecodeText text =
    case eitherDecodeStrict' (encodeUtf8 text) :: Either String a of
        Left err -> Left $ JSONDecodeError text (pack err)
        Right res -> Right res

-- | Exception for `jsonDecodeTextIO` function
--
data JSONDecodeTextIOException = JSONDecodeTextIOException
    { err :: JSONDecodeError -- ^ Decoding error
    }
instance Exception JSONDecodeTextIOException
instance Show JSONDecodeTextIOException where
    show e@(JSONDecodeTextIOException {err}) = errorShow e $ textShow err

-- | Exception for `jsonDecodeFile` function
--
data JSONDecodeFileException = JSONDecodeFileException
    { filePath :: Text -- ^ Specified file path
    , message :: Text -- ^ Decoding error
    }
instance Exception JSONDecodeFileException
instance Show JSONDecodeFileException where
    show e@(JSONDecodeFileException {filePath, message}) = errorShow e $
               "Error decoding JSON,"
            <> " path: [" <> filePath <> "],"
            <> " error: [" <> message <> "]"

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
-- Throws an exception if file cannot be read or data cannot be decoded
--
-- Arguments:
--
--    * @path :: Text@: Path to JSON file
--
-- Return value: Decoded data
--
jsonDecodeFile :: forall a . FromJSON a => Text -> IO a
jsonDecodeFile path =
    ioWithFileBytes path $ \bs ->
        case eitherDecode' bs :: Either String a of
            Left err -> throwIO $ JSONDecodeFileException path (pack err)
            Right res -> return res

-- | Error for `jsonGet` function
--
data JSONGetError = JSONGetError
    { objectField :: Text -- ^ Specified property
    , jsonValue :: Value -- ^ JSON value
    , message :: Text -- ^ Error message
    }
instance Show JSONGetError where
    show e@(JSONGetError {objectField, jsonValue, message}) = errorShow e $
               "Error accessing JSON property,"
            <> " name: [" <> objectField <> "],"
            <> " text: [" <> (Text.take 1024 $ jsonEncodeText jsonValue) <> "],"
            <> " error: [" <> message <> "]"

-- | Extract the field value from the specified JSON object
--
-- Returns an error, if specified JSON @Value@ is not a JSON object,
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
-- > let Right (fooval :: Int) = jsonGet obj "foo"
-- > let Right (barval :: Text) = jsonGet obj "bar"
-- >
--
-- Arguments:
--
--    * @field :: Text@: Field name
--    * @val :: Aeson.Value@: JSON value, must be a JSON object
--
-- Return value: Field value or an error
--
jsonGet :: forall a . FromJSON a => Text -> Value -> Either JSONGetError a
jsonGet field val =
    case val of
        Object obj -> case parseEither (.: field) obj :: Either String a of
            Left err -> Left $ JSONGetError field val (pack err)
            Right a -> Right a
        _ -> Left $ JSONGetError field val "Invalid non-object JSON value specified"

-- | JSON options with @unwrapUnaryRecords@ flag flipped to @True@
--
jsonUnwrapUnaryOptions :: Options
jsonUnwrapUnaryOptions =
    defaultOptions
        { unwrapUnaryRecords = True
        }
