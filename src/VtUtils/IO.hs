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
-- IO utilities
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module VtUtils.IO
    ( IOWithFileException(..)
    , ioWithFileBytes
    , ioWithFileText
    ) where

import Prelude (Either(..), IO, Show, ($), return, show)
import Control.Exception (Exception, SomeException(..), throwIO, try)
import qualified Data.ByteString.Lazy as ByteStringLazy
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy.Encoding (decodeUtf8With)
import qualified Data.Text.Lazy as TextLazy
import System.IO (IOMode(ReadMode), withBinaryFile)

import VtUtils.Error (errorShow)
import VtUtils.Text (textShow)

-- | Exception for `ioWithFileBytes` and `ioWithFileText` functions
--
data IOWithFileException = IOWithFileException
    { filePath :: Text -- ^ Specified path
    , exception :: SomeException -- ^ Exception record
    }
instance Exception IOWithFileException
instance Show IOWithFileException where
    show e@(IOWithFileException {filePath, exception}) = errorShow e $
               "Error reading file,"
            <> " path: [" <> filePath <> "],"
            <> " exception: [" <> textShow(exception) <> "]"

-- | Reads contents of a specified file as a lazy @ByteString@ (with streaming)
-- and provides it to the specified callback
--
-- Throws @IOWithFileBytesException@ if specified file cannot be read
--
-- Arguments:
--
--    * @path :: Text@: path to file
--    * @fun :: (Data.ByteString.Lazy.ByteString -> IO a)@: callback to process the file data
--
-- Return value: Result of the callback invocation
--
ioWithFileBytes :: Text -> (ByteStringLazy.ByteString -> IO a) -> IO a
ioWithFileBytes path fun = do
    outcome <- try $
        withBinaryFile (unpack path) ReadMode $ \ha -> do
            bs <- ByteStringLazy.hGetContents ha
            res <- fun bs
            return res
    case outcome of
        Left e -> throwIO $ IOWithFileException path e
        Right res -> return res

-- | Reads contents of a specified file as a lazy @Text@ (with streaming)
-- and provides it to the specified callback
--
-- File contents are decoded as @UTF-8@
--
-- Throws @IOWithFileBytesException@ if specified file cannot be read
--
-- Arguments:
--
--    * @path :: Text@: path to file
--    * @fun :: (Data.Text.Lazy.Text -> IO a)@: callback to process the file data
--
-- Return value: Result of the callback invocation
--
ioWithFileText :: Text -> (TextLazy.Text -> IO a) -> IO a
ioWithFileText path fun =
    ioWithFileBytes path $ \bs -> do
        let tx = decodeUtf8With lenientDecode bs
        res <- fun tx
        return res
