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

module MapTest ( mapTest ) where

import Test.HUnit
import Prelude (Int, ($), return)
import Data.HashMap.Strict (HashMap, fromList)
import Data.Text (Text)

import VtUtils.Map

testGet :: Test
testGet = TestLabel "testGet" $ TestCase $ do
    let map = fromList
            [ ("foo", 41)
            , ("bar", 42)
            ] :: HashMap Text Int
    assertEqual "get" 41 (mapGet map "foo")
    return ()

mapTest :: Test
mapTest = TestLabel "MapTest" $ TestList
    [ testGet
    ]
