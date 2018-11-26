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

module VtUtils.IO
    ( ioWithFileBytes
    , ioWithFileText
    ) where

import Prelude (IO, ($), return)
import Data.Text (Text, unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import System.IO (IOMode(ReadMode), withBinaryFile)

import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.Text.Lazy as TextLazy

ioWithFileBytes :: Text -> (ByteStringLazy.ByteString -> IO a) -> IO a
ioWithFileBytes path fun =
    withBinaryFile (unpack path) ReadMode $ \ha -> do
        bs <- ByteStringLazy.hGetContents ha
        res <- fun bs
        return res

ioWithFileText :: Text -> (TextLazy.Text -> IO a) -> IO a
ioWithFileText path fun =
    ioWithFileBytes path $ \bs -> do
        let tx = decodeUtf8 bs
        res <- fun tx
        return res
