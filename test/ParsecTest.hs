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
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module ParsecTest ( parsecTest ) where

import Test.HUnit
import Prelude (Either(..), ($), (<$>), (>>), return)
import Data.Either.Combinators (fromRight')
import Data.Text (Text, isPrefixOf, pack)
import Text.Parsec ((<|>), digit, many1, string)

import VtUtils.Parsec
import VtUtils.Text

tx :: Text
tx = "foo 41\n42 bar\nbaz 43\n"

testLineContains :: Test
testLineContains = TestLabel "testLineContains" $ TestCase $ do
    assertEqual "41" "foo 41" $ fromRight' $ parsecParseText (parsecLineContains "41") tx
    assertEqual "42" "42 bar" $ fromRight' $ parsecParseText (parsecLineContains "42") tx
    assertEqual "43" "baz 43" $ fromRight' $ parsecParseText (parsecLineContains "43") tx
    return ()

testLinePrefix :: Test
testLinePrefix = TestLabel "testLinePrefix" $ TestCase $ do
    assertEqual "42" "42 bar" $ fromRight' $ parsecParseText (parsecLinePrefix "42") tx
    return ()

testLineNoPrefix :: Test
testLineNoPrefix = TestLabel "testLineNoPrefix" $ TestCase $ do
    let parser = parsecLineNoPrefix "foo"
    assertEqual "42 line" "42 bar" $ fromRight' $ parsecParseText parser tx
    return ()

testSkipLines :: Test
testSkipLines = TestLabel "testSkipLines" $ TestCase $ do
    let parser = parsecSkipLines 1 >> many1 digit
    assertEqual "42" "42" $ fromRight' $ parsecParseText parser tx
    return ()

testSkipManyTill :: Test
testSkipManyTill = TestLabel "testSkipManyTill" $ TestCase $ do
    let parser = parsecSkipManyTill "42" >> many1 digit
    assertEqual "42" "42" $ fromRight' $ parsecParseText parser tx
    return ()

testTry :: Test
testTry = TestLabel "testTry" $ TestCase $ do
    let parser = parsecTry (string "bar") <|> string "foo"
    assertEqual "foo" "foo" $ fromRight' $ parsecParseText parser tx
    return ()

testWhitespace :: Test
testWhitespace = TestLabel "testWhitespace" $ TestCase $ do
    let parser = string "foo 41" >> parsecWhitespace >> many1 digit
    assertEqual "42" "42" $ fromRight' $ parsecParseText parser tx
    return ()

testErrorToText :: Test
testErrorToText = TestLabel "testErrorToText" $ TestCase $ do
    case parsecParseText (string "bar") tx of
        Right _ -> assertFailure "Parser must fail"
        Left e -> do
            let etx = textShow e
            assertBool "err message" $ isPrefixOf "ParsecParseTextError" etx
    return ()

testParseFile :: Test
testParseFile = TestLabel "testParseFile" $ TestCase $ do
    let parser = pack <$> string "foo"
    res <- parsecParseFile parser "test/data/test.txt"
    assertEqual "foo" "foo" res
    return ()

testParseText :: Test
testParseText = TestLabel "testParseText" $ TestCase $ do
    let parser = pack <$> string "foo"
    assertEqual "foo" "foo" $ fromRight' $ parsecParseText parser "foo"
    return ()

parsecTest :: Test
parsecTest = TestLabel "ParsecTest" $ TestList
    [ testLineContains
    , testLinePrefix
    , testLineNoPrefix
    , testSkipLines
    , testSkipManyTill
    , testTry
    , testWhitespace
    , testErrorToText
    , testParseFile
    , testParseText
    ]

