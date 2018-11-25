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

module DateTest ( dateTest ) where

import Test.HUnit
import Prelude (($), return)
import Data.Text (Text)
import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar (fromGregorian)

import VtUtils.Date

dt :: UTCTime
dt = UTCTime (fromGregorian 2018 11 25) 1

dts :: Text
dts = "2018-11-25 00:00:01"

testFormat :: Test
testFormat = TestLabel "testFormat" $ TestCase $ do
    assertEqual "year" "2018" (dateFormat "%Y" dt)
    assertEqual "month" "11" (dateFormat "%m" dt)
    assertEqual "day" "25" (dateFormat "%d" dt)
    assertEqual "second" "01" (dateFormat "%S" dt)
    return ()

testFormatISO8601 :: Test
testFormatISO8601 = TestLabel "testFormatISO8601" $ TestCase $ do
    assertEqual "iso" dts (dateFormatISO8601 dt)
    return ()

testParseISO8601 :: Test
testParseISO8601 = TestLabel "testParseISO8601" $ TestCase $ do
    let parsed = dateParseISO8601 dts
    assertEqual "parsed" dt parsed
    return ()

dateTest :: Test
dateTest = TestLabel "DateTest" (TestList
    [ testFormat
    , testFormatISO8601
    , testParseISO8601
    ])

