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
-- Custom Prelude
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module VtUtils.Prelude
    ( Bool(True, False), Either(Left, Right), Eq, Int, IO, Maybe(Just, Nothing), Read, Show, String
    , (+), (-), (*), (/), (>), (.), ($), (<)
    , (>=), (<=), (==), (/=), ($!), (>>)
    , (>>=), (&&), (||), (<$>), (<*>)
    , abs, div, error, flip, fmap, fromIntegral, id, length, mapM, mapM_, mod
    , not, otherwise, pure, read, return, seq, sequence, sequence_, show, undefined
    -- Control.Exception
    , SomeException
    , bracket, bracket_, throw, try
    -- Control.Monad
    , forM, forM_, mfilter, unless, when
    -- Control.Monad.ST
    , runST
    -- Data.Aeson
    , FromJSON, ToJSON, Value
    , (.=)
    , object, parseJSON, toJSON
    -- Data.ByteString
    , ByteString
    -- Data.Foldable
    , foldl', foldr'
    -- Data.HashMap.Strict
    , HashMap
    , lookup
    -- Data.Int
    , Int64
    -- Data.Maybe
    , fromJust, isJust
    -- Data.Monoid
    , (<>)
    , mconcat
    -- Data.Text
    , Text
    , pack, unpack
    -- Data.Text.Encoding
    , decodeUtf8, encodeUtf8
    -- Data.Text.IO
    , appendFile, putStrLn, readFile, writeFile
    -- Data.Text.Lazy
    , toStrict
    -- Data.Text.Lazy.Builder
    , Builder, fromText, fromLazyText, fromString, toLazyText
    -- Data.Time.Clock
    , UTCTime
    , getCurrentTime
    -- Data.Typeable
    , Typeable
    , cast
    -- Data.Vector
    , Vector
    , (!)
    , fromList, ifoldl', toList
    -- Debug.Trace
    , trace
    -- Foreign.C.String
    , CString
    -- GHC.Generics
    , Generic
    -- VtUtils.Date
    , dateFormat, dateFormatISO8601, dateParseISO8601
    -- VtUtils.FS
    , fsCopyDirectory
    -- VtUtils.IO
    , ioWithFileBytes, ioWithFileText
    -- VtUtils.Json
    , jsonDecodeFile,jsonDecodeText, jsonEncodeText, jsonGet
    -- VtUtils.Map
    , mapGet
    -- VtUtils.Parsec
    , Parser
    , parsecLineContains, parsecLinePrefix, parsecLineNoPrefix, parsecSkipLines, parsecSkipManyTill
    , parsecTry, parsecWhitespace, parsecErrorToText, parsecParseFile, parsecParseText
    -- VtUtils.Path
    , pathIsAbsolute, pathConcat, pathPrepend
    -- VtUtils.Text
    , textShow
    ) where

import Prelude
    ( Bool(True, False), Either(Left, Right), Eq, Int, IO, Maybe(Just, Nothing), Read, Show, String
    , (+), (-), (*), (/), (>), (.), ($), (<)
    , (>=), (<=), (==), (/=), ($!), (>>)
    , (>>=), (&&), (||), (<$>), (<*>)
    , abs, div, error, flip, fmap, fromIntegral, id, length, mapM, mapM_, mod
    , not, otherwise, pure, read, return, seq, sequence, sequence_, show, undefined
    )
import Control.Exception (SomeException, bracket, bracket_, throw, try)
import Control.Monad (forM, forM_, mfilter, unless, when)
import Control.Monad.ST (runST)
import Data.Aeson (FromJSON, ToJSON, Value, (.=), object, parseJSON, toJSON)
import Data.ByteString (ByteString)
import Data.Foldable (foldl', foldr')
import Data.HashMap.Strict (HashMap, lookup)
import Data.Int (Int64)
import Data.Maybe (fromJust, isJust)
import Data.Monoid ((<>), mconcat)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.IO (appendFile, putStrLn, readFile, writeFile)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (Builder, fromText, fromLazyText, fromString, toLazyText)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Typeable (Typeable, cast)
import Data.Vector (Vector, (!), fromList, ifoldl', toList)
import Debug.Trace (trace)
import Foreign.C.String (CString)
import GHC.Generics (Generic)

import VtUtils.Date (dateFormat, dateFormatISO8601, dateParseISO8601)
import VtUtils.FS (fsCopyDirectory)
import VtUtils.IO (ioWithFileBytes, ioWithFileText)
import VtUtils.Json (jsonDecodeFile,jsonDecodeText, jsonEncodeText, jsonGet)
import VtUtils.Map (mapGet)
import VtUtils.Parsec (Parser, parsecLineContains, parsecLinePrefix, parsecLineNoPrefix, parsecSkipLines
    , parsecSkipManyTill, parsecTry, parsecWhitespace, parsecErrorToText, parsecParseFile, parsecParseText )
import VtUtils.Path (pathIsAbsolute, pathConcat, pathPrepend)
import VtUtils.Text (textShow)
