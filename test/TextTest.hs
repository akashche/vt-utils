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
import Prelude (String, ($))
import Data.ByteString (ByteString)
import Data.Text (Text)

import VtUtils.Text

testShow :: Test
testShow = TestLabel "testShow" $ TestCase $ do
    assertEqual "text" "foo" (textShow ("foo" :: Text))
    assertEqual "string" "foo" (textShow ("foo" :: String))
    assertEqual "bytestring" "foo" (textShow ("foo" :: ByteString))

textTest :: Test
textTest = TestLabel "TextTest" (TestList
    [ testShow
    ])
