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

module PathTest ( pathTest ) where

import Test.HUnit
import Prelude (($), not, return)

import VtUtils.Path

testIsAbsolute :: Test
testIsAbsolute = TestLabel "testIsAbsolute" $ TestCase $ do
    assertBool "abs1" (pathIsAbsolute "/foo/bar")
    assertBool "abs2" (pathIsAbsolute "c:/foo/bar")
    assertBool "abs3" (pathIsAbsolute "c:\\foo\\bar")
    assertBool "abs4" (not (pathIsAbsolute "foo/bar"))
    assertBool "abs5" (not (pathIsAbsolute "foo/bar"))
    return ()

testConcat :: Test
testConcat = TestLabel "testConcat" $ TestCase $ do
    assertEqual "concat1" "foo/bar" (pathConcat "foo" "bar")
    assertEqual "concat2" "foo/bar" (pathConcat "foo/" "bar")
    assertEqual "concat3" "foo" (pathConcat "foo" "")
    assertEqual "concat4" "foo" (pathConcat "" "foo")
    return ()

testPrepend :: Test
testPrepend = TestLabel "testPrepend" $ TestCase $ do
    assertEqual "prepend1" "/foo/bar" (pathPrepend "/foo" "bar")
    assertEqual "prepend2" "/foo/bar" (pathPrepend "/foo/" "bar")
    assertEqual "prepend3" "/bar" (pathPrepend "/foo" "/bar")
    assertEqual "prepend4" "c:\\bar" (pathPrepend "/foo" "c:\\bar")
    assertEqual "prepend5" "c:/bar" (pathPrepend "/foo" "c:/bar")
    return ()

pathTest :: Test
pathTest = TestLabel "PathTest" $ TestList
    [ testIsAbsolute
    , testConcat
    , testPrepend
    ]
