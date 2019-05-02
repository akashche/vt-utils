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

module QueriesTest ( queriesTest ) where

import Test.HUnit
import Prelude (return, ($))
import Data.HashMap.Strict (lookup)
import Data.Maybe (fromJust)
import qualified Data.HashMap.Strict as HashMap

import VtUtils.Queries

testLoad :: Test
testLoad = TestLabel "testLoad" $ TestCase $ do
    qrs <- queriesLoad "test/data/test.sql"
    assertEqual "count" 2 (HashMap.size qrs)
    assertEqual "foo" "select foo\nfrom bar" (fromJust $ lookup "selectFoo" qrs)
    assertEqual "foo" "update bar\nset foo = 42" (fromJust $ lookup "updateBar" qrs)
    return ()

queriesTest :: Test
queriesTest = TestLabel "QueriesTest" $ TestList
    [ testLoad
    ]
