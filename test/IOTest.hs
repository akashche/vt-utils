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

module IOTest ( ioTest ) where

import Test.HUnit
import Prelude (($), (<$>), return)
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO (readFile)

import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.Text.Lazy as TextLazy

import VtUtils.IO

testWithFileBytes :: Test
testWithFileBytes = TestLabel "testWithFileBytes" $ TestCase $ do
    lazy <- decodeUtf8 <$> ioWithFileBytes "ChangeLog.md" (\bs -> return (ByteStringLazy.toStrict bs))
    strict <- readFile (unpack "ChangeLog.md")
    assertEqual "bytes" strict lazy
    return ()

testWithFileText :: Test
testWithFileText = TestLabel "testWithFileText" $ TestCase $ do
    lazy <- ioWithFileText "ChangeLog.md" (\tx -> return (TextLazy.toStrict tx))
    strict <- readFile (unpack "ChangeLog.md")
    assertEqual "text" strict lazy
    return ()

ioTest :: Test
ioTest = TestLabel "IOTest" (TestList
    [ testWithFileBytes
    , testWithFileText
    ])
