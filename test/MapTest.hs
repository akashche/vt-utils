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
{-# LANGUAGE Strict #-}

module MapTest ( mapTest ) where

import Test.HUnit
import Prelude (Int, Maybe(..), ($), return)
import Data.HashMap.Strict (lookup)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector, fromList)

import VtUtils.Map

testFromVector :: Test
testFromVector = TestLabel "testFromVector" $ TestCase $ do
    let vec = fromList
            [ ("foo", 41)
            , ("bar", 42)
            , ("foo", 43)
            ] :: Vector (Text, Int)
    -- duplicate
    let map1 = mapFromVector vec $ \_ (lab, _) -> Text.toUpper lab
    assertEqual "length" 2 $ HashMap.size map1
    assertEqual "el 1" (Just ("bar", 42)) $ lookup "BAR" map1
    assertEqual "el 2" (Just ("foo", 43)) $ lookup "FOO" map1
    -- idx
    let map2 = mapFromVector vec $ \idx _ -> idx
    assertEqual "length" 3 $ HashMap.size map2
    assertEqual "el 1" (Just ("foo", 41)) $ lookup 0 map2
    assertEqual "el 1" (Just ("bar", 42)) $ lookup 1 map2
    assertEqual "el 2" (Just ("foo", 43)) $ lookup 2 map2
    return ()

mapTest :: Test
mapTest = TestLabel "MapTest" $ TestList
    [ testFromVector
    ]
