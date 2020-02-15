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

module HUnitTest ( hunitTest ) where

import Test.HUnit
import Prelude (Int, IO, (+), ($), return)
import Control.Exception (SomeException)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Text as Text
import Data.Vector (fromList, singleton)

import VtUtils.HUnit
import VtUtils.Text

testMain :: Test
testMain = TestLabel "testMain" $ TestCase $ do
    -- uses args, hard to test
    return ()

testRun :: Test
testRun = TestLabel "testRun" $ TestCase $ do
    ref <- newIORef (0 :: Int)
    let tst = TestCase $ do
            val <- readIORef ref
            _ <- writeIORef ref (val + 1)
            return ()
    hunitRun $ fromList
        [ TestLabel "group1" $ TestList
            [ TestLabel "test1" tst
            ]
        ]
    after <- readIORef ref
    assertEqual "test case called" 1 after
    return ()

testRunGroup :: Test
testRunGroup = TestLabel "testRunGroup" $ TestCase $ do
    -- check success
    ref <- newIORef (0 :: Int)
    before <- readIORef ref
    assertEqual "before" 0 before
    let tst = TestCase $ do
            val <- readIORef ref
            _ <- writeIORef ref (val + 1)
            return ()
    hunitRunGroup (fromList
        [ TestLabel "group1" $ TestList
            [ TestLabel "test1" tst
            ]
        , TestLabel "group2" $ TestList
            [ TestLabel "test1" tst
            ]
        ]) "group1"
    after <- readIORef ref
    assertEqual "specified group called" 1 after

    -- check no label
    errnolabel <- hunitCatchException "no label" $ hunitRunGroup (singleton tst) "" :: IO SomeException
    assertBool "no label msg" $ Text.isPrefixOf "HUnitGroupLabelNotSpecifiedException" $ textShow errnolabel

    -- check duplicate group
    errdup <- hunitCatchException "dup" $
        hunitRunGroup (fromList
            [ TestLabel "group1" tst
            , TestLabel "group1" tst
            ]) "" :: IO SomeException
    assertBool "dup msg" $ Text.isPrefixOf "HUnitDuplicateGroupLabelException" $ textShow errdup

    -- check missed label
    errlabelnf <- hunitCatchException "not found" $ hunitRunGroup (fromList []) "group1" :: IO SomeException
    assertBool "not found" $ Text.isPrefixOf "HUnitLabelNotFoundException" $ textShow errlabelnf

    return ()

testRunSingle :: Test
testRunSingle = TestLabel "testRunSingle" $ TestCase $ do
    -- check success
    ref <- newIORef (0 :: Int)
    before <- readIORef ref
    assertEqual "before" 0 before
    let tst = TestCase $ do
            val <- readIORef ref
            _ <- writeIORef ref (val + 1)
            return ()
    hunitRunSingle (fromList
        [ TestLabel "group1" $ TestList
            [ TestLabel "test1" tst
            , TestLabel "test2" tst
            ]
        ]) "group1" "test1"
    after <- readIORef ref
    assertEqual "specified test called" 1 after

    -- check non-list group
    errnolist <- hunitCatchException "no list" $
        hunitRunSingle (fromList
            [ TestLabel "group1" tst
            ]) "group1" "" :: IO SomeException
    assertBool "nolist" $ Text.isPrefixOf "HUnitNonListGroupException" $ textShow errnolist

    -- check missed label
    errlabelnf <- hunitCatchException "not found" $
        hunitRunSingle (fromList
            [ TestLabel "group1" $ TestList
                [ TestLabel "test1" tst
                ]
            ]) "group1" "test2" :: IO SomeException
    assertBool "not found" $ Text.isPrefixOf "HUnitLabelNotFoundException" $ textShow errlabelnf
    return ()

hunitTest :: Test
hunitTest = TestLabel "HUnitTest" $ TestList
    [ testMain
    , testRun
    , testRunGroup
    , testRunSingle
    ]

