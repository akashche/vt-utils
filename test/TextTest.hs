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

module TextTest ( textTest ) where

import Test.HUnit
import Prelude (String, ($), return)
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Text (Text, drop)
import Data.Vector (foldl', fromList)

import VtUtils.Text

testShow :: Test
testShow = TestLabel "testShow" $ TestCase $ do
    assertEqual "text" "foo" (textShow ("foo" :: Text))
    assertEqual "string" "foo" (textShow ("foo" :: String))
    assertEqual "bytestring" "foo" (textShow ("foo" :: ByteString))

testSplit :: Test
testSplit = TestLabel "testSplit" $ TestCase $ do
    assertEqual "1" "foo_bar_baz_boo" $ conc $ textSplit "foo{}bar{}baz{}boo" "{}"
    assertEqual "2" "" $ conc $ textSplit "" "{}"
    assertEqual "3" "foo" $ conc $ textSplit "foo" "{}"
    assertEqual "4" "_foo" $ conc $ textSplit "{}foo" "{}"
    assertEqual "5" "foo_" $ conc $ textSplit "foo{}" "{}"
    assertEqual "6" "_foo_bar__baz__boo_" $ conc $ textSplit "{}foo{}bar{}{}baz{}{}boo{}" "{}"
    return ()
    where
        conc vec = drop 1 $ foldl' fun "" vec
        fun ac el = ac <> "_" <> el

testFormat :: Test
testFormat = TestLabel "testFormat" $ TestCase $ do
    assertEqual "1" "foo 41 bar 42" $ textFormat "foo {} bar {}" (fromList ["41", "42"])
    assertEqual "2" "43 foo 41 bar 42" $ textFormat "{} foo {} bar {}" (fromList ["43", "41", "42"])
    assertEqual "3" "foo 42 bar" $ textFormat "foo {} bar" (fromList ["42"])
    assertEqual "2" "43 foo 4142 bar 44" $ textFormat "{} foo {}{} bar {}" (fromList ["43", "41", "42", "44"])
    return ()

textTest :: Test
textTest = TestLabel "TextTest" $ TestList
    [ testShow
    , testSplit
    , testFormat
    ]
