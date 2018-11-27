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

import VtUtils.Parsec

testInt :: Test
testInt = TestLabel "testInt" $ TestCase $ do
    assertEqual "int" 42 (parsecParseText parsecInt "42")
    return ()

testLineContains :: Test
testLineContains = TestLabel "testLineContains" $ TestCase $ do
    let tx = "foo 41\n42 bar\nbaz 43\n"
    assertEqual "41" "foo 41" (parsecParseText (parsecLineContains "41") tx)
    assertEqual "42" "42 bar" (parsecParseText (parsecLineContains "42") tx)
    assertEqual "43" "baz 43" (parsecParseText (parsecLineContains "43") tx)
    return ()

testLinePrefix :: Test
testLinePrefix = TestLabel "testLinePrefix" $ TestCase $ do
--  TODO
    return ()

testSkipLines :: Test
testSkipLines = TestLabel "testSkipLines" $ TestCase $ do
--  TODO
    return ()

testSkipLinesPrefix :: Test
testSkipLinesPrefix = TestLabel "testSkipLinesPrefix" $ TestCase $ do
--  TODO
    return ()

testSkipLinesTill :: Test
testSkipLinesTill = TestLabel "testSkipLinesTill" $ TestCase $ do
--  TODO
    return ()

testSkipManyTill :: Test
testSkipManyTill = TestLabel "testSkipManyTill" $ TestCase $ do
--  TODO
    return ()

testSkipOne :: Test
testSkipOne = TestLabel "testSkipOne" $ TestCase $ do
--  TODO
    return ()

testTry :: Test
testTry = TestLabel "testTry" $ TestCase $ do
--  TODO
    return ()

testWhitespace :: Test
testWhitespace = TestLabel "testWhitespace" $ TestCase $ do
--  TODO
    return ()

testErrorToText :: Test
testErrorToText = TestLabel "testErrorToText" $ TestCase $ do
--  TODO
    return ()

testParseFile :: Test
testParseFile = TestLabel "testParseFile" $ TestCase $ do
--  TODO
    return ()

testParseText :: Test
testParseText = TestLabel "testParseText" $ TestCase $ do
--  TODO
    return ()

parsecTest :: Test
parsecTest = TestLabel "ParsecTest" (TestList
    [ testInt
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

