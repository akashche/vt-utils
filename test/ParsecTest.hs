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

module ParsecTest ( parsecTest ) where

import Test.HUnit
import Prelude (($), return)

-- import VtUtils.Parsec

testFloatAsInt :: Test
testFloatAsInt = TestLabel "testFloatAsInt" $ TestCase $ do
    return ()

testInt :: Test
testInt = TestLabel "testInt" $ TestCase $ do
    return ()

testLineContains :: Test
testLineContains = TestLabel "testLineContains" $ TestCase $ do
    return ()

testLinePrefix :: Test
testLinePrefix = TestLabel "testLinePrefix" $ TestCase $ do
    return ()

testSkipLines :: Test
testSkipLines = TestLabel "testSkipLines" $ TestCase $ do
    return ()

testSkipLinesPrefix :: Test
testSkipLinesPrefix = TestLabel "testSkipLinesPrefix" $ TestCase $ do
    return ()

testSkipLinesTill :: Test
testSkipLinesTill = TestLabel "testSkipLinesTill" $ TestCase $ do
    return ()

testSkipManyTill :: Test
testSkipManyTill = TestLabel "testSkipManyTill" $ TestCase $ do
    return ()

testSkipOne :: Test
testSkipOne = TestLabel "testSkipOne" $ TestCase $ do
    return ()

testTry :: Test
testTry = TestLabel "testTry" $ TestCase $ do
    return ()

testWhitespace :: Test
testWhitespace = TestLabel "testWhitespace" $ TestCase $ do
    return ()

testErrorToText :: Test
testErrorToText = TestLabel "testErrorToText" $ TestCase $ do
    return ()

testParseFile :: Test
testParseFile = TestLabel "testParseFile" $ TestCase $ do
    return ()

testParseText :: Test
testParseText = TestLabel "testParseText" $ TestCase $ do
    return ()

parsecTest :: Test
parsecTest = TestLabel "ParsecTest" (TestList
    [ testFloatAsInt
    , testInt
    , testLineContains
    , testLinePrefix
    , testSkipLines
    , testSkipLinesPrefix
    , testSkipLinesTill
    , testSkipManyTill
    , testSkipOne
    , testTry
    , testWhitespace
    , testErrorToText
    , testParseFile
    , testParseText
    ])

