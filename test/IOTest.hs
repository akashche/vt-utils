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
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module IOTest ( ioTest ) where

import Test.HUnit
import Prelude (IO, ($), (<$>), return)
import Control.Exception (SomeException)
import qualified Data.ByteString.Lazy as ByteStringLazy
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextLazy

import VtUtils.HUnit
import VtUtils.IO
import VtUtils.Text

testWithFileBytes :: Test
testWithFileBytes = TestLabel "testWithFileBytes" $ TestCase $ do
    let bsfun bs = return $ ByteStringLazy.toStrict bs
    -- success
    tx <- decodeUtf8 <$> ioWithFileBytes "test/data/test.txt" bsfun
    assertEqual "bytes" "foo" tx
    -- fail
    err <- hunitCatchException "file not found" $ ioWithFileBytes "test/data/fail.txt" bsfun :: IO SomeException
    assertBool "file not found" $ Text.isPrefixOf "IOWithFileException" $ textShow err
    return ()

testWithFileText :: Test
testWithFileText = TestLabel "testWithFileText" $ TestCase $ do
    let txfun tx = return $ TextLazy.toStrict tx
    -- success
    tx <- ioWithFileText "test/data/test.txt" txfun
    assertEqual "text" "foo" tx
    -- fail
    err <- hunitCatchException "file not found" $ ioWithFileText "test/data/fail.txt" txfun :: IO SomeException
    assertBool "file not found" $ Text.isPrefixOf "IOWithFileException" $ textShow err
    return ()

ioTest :: Test
ioTest = TestLabel "IOTest" $ TestList
    [ testWithFileBytes
    , testWithFileText
    ]
