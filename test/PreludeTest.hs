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

module PreludeTest ( preludeTest ) where

import Test.HUnit
import Prelude ()
import VtUtils.Prelude

test1 :: Test
test1 = TestLabel "test" $ TestCase $ do
    -- only check that Prelude can be imported
    assertEqual "prelude" (43 :: Int) (42 + 1)

preludeTest :: Test
preludeTest = TestLabel "PreludeTest" (TestList
    [ test1
    ])
