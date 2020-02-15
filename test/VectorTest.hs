--
-- Copyright 2019, akashche at redhat.com
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

module VectorTest ( vectorTest ) where

import Test.HUnit
import Prelude (Int, Maybe(..), ($), return)
import Data.Text (Text)
import Data.Vector (fromList)

import VtUtils.Text
import VtUtils.Vector

keyfun :: Int -> Int -> Text
keyfun _idx el = textShow el

testFirstDuplicate :: Test
testFirstDuplicate = TestLabel "testFirstDuplicate" $ TestCase $ do
    assertEqual "dup 1" (Just (1, 2)) $ vectorFirstDuplicate (fromList [41, 42, 42, 43]) keyfun
    assertEqual "dup 2" (Just (0, 1)) $ vectorFirstDuplicate (fromList [42, 42, 42, 42]) keyfun
    assertEqual "dup 3" (Just (2, 3)) $ vectorFirstDuplicate (fromList [40, 41, 42, 42]) keyfun
    assertEqual "dup 4" (Just (0, 3)) $ vectorFirstDuplicate (fromList [42, 40, 41, 42]) keyfun
    assertEqual "dup fail" Nothing $ vectorFirstDuplicate (fromList [41, 42, 43, 44]) keyfun
    return ()

vectorTest :: Test
vectorTest = TestLabel "VectorTest" $ TestList
    [ testFirstDuplicate
    ]

