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
    , abs, ceiling, div, error, flip, floor, fmap, fromIntegral, fst, id, length, mapM, mapM_, mod
    , not, otherwise, pure, read, return, seq, sequence, sequence_, show, snd, take, undefined
    -- Control.Concurrent
    , forkIO, forkOS
    -- Control.Exception
    , SomeException
    , bracket, bracket_, throw, try
    -- Control.Monad
    , forM, forM_, mfilter, unless, when
    -- Control.Monad.IO.Class
    , liftIO
    -- Control.Monad.ST
    , runST
    -- Control.Monad.Trans.Class
    , lift
    -- Data.Aeson
    , FromJSON, ToJSON, Value
    , (.=)
    , object, parseJSON, toJSON
    -- Data.ByteString
    , ByteString
    , packCString, packCStringLen
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
    , appendFile, getLine, putStrLn, readFile, writeFile
    -- Data.Text.Lazy
    , toStrict
    -- Data.Text.Lazy.Builder
    , Builder, fromText, fromLazyText, fromString, toLazyText
    -- Data.Time.Clock
    , UTCTime
    , getCurrentTime
    -- Data.Time.Clock.POSIX
    , POSIXTime
    , posixSecondsToUTCTime, utcTimeToPOSIXSeconds
    -- Data.Typeable
    , Typeable
    , cast
    -- Data.Vector
    , Vector
    , (!)
    , fromList, ifoldl', toList
    -- Data.Word
    , Word(..), Word8(..), Word16(..), Word32(..), Word64(..)
    -- Debug.Trace
    , trace
    -- Foreign
    , Ptr
    , castPtr, newForeignPtr_, nullPtr, peek, poke, plusPtr, ptrToIntPtr
    -- Foreign.C.String
    , CString, CStringLen
    -- Foreign.C.Types
    , CChar(..), CInt(..), CLong(..), CShort(..), CSize(..), CUChar(..), CUInt(..), CULong(..), CUShort(..)
    -- Foreign.Marshal.Utils
    , copyBytes
    -- Foreign.Storable
    , Storable
    , alignment, peekByteOff, pokeByteOff, sizeOf
    -- GHC.Generics
    , Generic
    -- Network.Wai
    , Application, Request, RequestBodyLength(..)
    , lazyRequestBody, queryString, rawPathInfo, requestBody, requestBodyLength, requestHeaders, requestMethod, responseLBS
    -- System.Environment
    , getArgs

    -- VtUtils.Date
    , dateFormat, dateFormatISO8601, dateParseISO8601
    -- VtUtils.FFI
    , ffiWithPtr, ffiWithPtrPtr, ffiWithUTF8, ffiWithUTF16
    -- VtUtils.FS
    , fsCopyDirectory
    -- VtUtils.HTTP
    , httpContentTypeJSON, httpRequestPath, httpRequestBodyText, httpRequestBodyJSON
    , httpRequestHeaders, httpRequestHeadersMap
    -- VtUtils.IO
    , ioWithFileBytes, ioWithFileText
    -- VtUtils.JSON
    , jsonDecodeFile,jsonDecodeText, jsonEncodeText, jsonGet
    -- VtUtils.Map
    , mapGet
    -- VtUtils.Parsec
    , Parser
    , parsecLineContains, parsecLinePrefix, parsecLineNoPrefix, parsecSkipLines, parsecSkipManyTill
    , parsecTry, parsecWhitespace, parsecErrorToText, parsecParseFile, parsecParseText
    -- VtUtils.Path
    , pathIsAbsolute, pathConcat, pathPrepend
    -- VtUtils.Process
    , processSpawnAndWait
    -- VtUtils.Text
    , textShow
    ) where

import Prelude
    ( Bool(True, False), Either(Left, Right), Eq, Int, IO, Maybe(Just, Nothing), Read, Show, String
    , (+), (-), (*), (/), (>), (.), ($), (<)
    , (>=), (<=), (==), (/=), ($!), (>>)
    , (>>=), (&&), (||), (<$>), (<*>)
    , abs, ceiling, div, error, flip, floor, fmap, fromIntegral, fst, id, length, mapM, mapM_, mod
    , not, otherwise, pure, read, return, seq, sequence, sequence_, snd, show, take, undefined
    )
import Control.Concurrent (forkIO, forkOS)
import Control.Exception (SomeException, bracket, bracket_, throw, try)
import Control.Monad (forM, forM_, mfilter, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.ST (runST)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON, ToJSON, Value, (.=), object, parseJSON, toJSON)
import Data.ByteString (ByteString, packCString, packCStringLen)
import Data.Foldable (foldl', foldr')
import Data.HashMap.Strict (HashMap, lookup)
import Data.Int (Int64)
import Data.Maybe (fromJust, isJust)
import Data.Monoid ((<>), mconcat)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.IO (appendFile, getLine, putStrLn, readFile, writeFile)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (Builder, fromText, fromLazyText, fromString, toLazyText)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Typeable (Typeable, cast)
import Data.Vector (Vector, (!), fromList, ifoldl', toList)
import Data.Word (Word(..), Word8(..), Word16(..), Word32(..), Word64(..))
import Debug.Trace (trace)
import Foreign (Ptr, castPtr, newForeignPtr_, nullPtr, peek, poke, plusPtr, ptrToIntPtr)
import Foreign.C.String (CString, CStringLen)
import Foreign.C.Types (CChar(..), CInt(..), CLong(..), CShort(..), CSize(..), CUChar(..), CUInt(..), CULong(..), CUShort(..))
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Storable (Storable, alignment, peekByteOff, pokeByteOff, sizeOf)
import GHC.Generics (Generic)
import Network.Wai (Application, Request, RequestBodyLength(..)
    , lazyRequestBody, queryString, rawPathInfo, requestBody, requestBodyLength, requestHeaders, requestMethod, responseLBS)
import System.Environment (getArgs)

import VtUtils.Date (dateFormat, dateFormatISO8601, dateParseISO8601)
import VtUtils.FFI (ffiWithPtr, ffiWithPtrPtr, ffiWithUTF8, ffiWithUTF16)
import VtUtils.FS (fsCopyDirectory)
import VtUtils.HTTP (httpContentTypeJSON, httpRequestPath, httpRequestBodyText, httpRequestBodyJSON
    , httpRequestHeaders, httpRequestHeadersMap)
import VtUtils.IO (ioWithFileBytes, ioWithFileText)
import VtUtils.JSON (jsonDecodeFile,jsonDecodeText, jsonEncodeText, jsonGet)
import VtUtils.Map (mapGet)
import VtUtils.Parsec (Parser, parsecLineContains, parsecLinePrefix, parsecLineNoPrefix, parsecSkipLines
    , parsecSkipManyTill, parsecTry, parsecWhitespace, parsecErrorToText, parsecParseFile, parsecParseText )
import VtUtils.Path (pathIsAbsolute, pathConcat, pathPrepend)
import VtUtils.Process (processSpawnAndWait)
import VtUtils.Text (textShow)
