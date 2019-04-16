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
--
-- |
-- HUnit utilities
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module VtUtils.HUnit
    ( hunitCatchException
    , hunitMain
    , hunitRun
    , hunitRunGroup
    , hunitRunSingle
    ) where

import Prelude (Bool, Either(..), IO, Show(..), (==), (/=), (.), ($), (<$>), error, fmap, return)
import Control.Exception (Exception, throwIO, try)
import Control.Monad (when)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import Data.Vector (Vector, (!), filter, fromList, length, toList)
import Test.HUnit (Test(..), runTestTT, testCasePaths)
import System.Environment (getArgs)

import VtUtils.Text (textShow)

labelFilter :: Text -> Test -> Bool
labelFilter grlabel gr  =
    case gr of
        (TestLabel label _) -> ((pack label) == grlabel)
        _ -> error . unpack $
               "Invalid test group,"
            <> " label: [" <> grlabel <>"]"
            <> " paths: [" <> textShow (testCasePaths gr) <> "]"

-- | Runs all HUnit tests from a specified Vector
--
-- Tests results are printed to stdout
--
-- Arguments:
--
--    * @tests :: Vector Test@: HUnit tests to run
--
hunitRun :: Vector Test -> IO ()
hunitRun tests = do
    _ <- runTestTT (TestList (toList tests))
    return ()

-- | Runs a subset of @HUnit@ tests with a specified label value
--
-- Throws an error if no tests in the specified @Vector@ have specified label
--
-- Tests results are printed to stdout
--
-- Arguments:
--
--    * @tests :: Vector Test@: HUnit tests
--    * @label :: Text@: Group label
--
hunitRunGroup :: Vector Test -> Text -> IO ()
hunitRunGroup tests label = do
    let grtests = (filter (labelFilter label) tests)
    when (0 == (length grtests)) $ error . unpack $
        "Test group not found, label: [" <> label <> "]"
    when (1 /= (length grtests)) $ error . unpack $
        "Invalid duplicated group, label: [" <> label <> "]"
    hunitRun grtests

-- | Runs a single test from a specified @Vector@ of @HUnit@ tests
--
-- Throws an error if a test with a specified group label and test label is not found
-- in specified Vector of tests
--
-- Tests results are printed to stdout
--
-- Arguments:
--
--    * @tests :: Vector Test@: HUnit tests
--    * @grlabel :: Text@: Group label
--    * @tslabel :: Text@: Test label
--
hunitRunSingle :: Vector Test -> Text -> Text -> IO ()
hunitRunSingle tests grlabel tslabel = do
    let grtests = (filter (labelFilter grlabel) tests)
    when (0 == (length grtests)) $ error . unpack $
        "Test group not found, label: [" <> grlabel <> "]"
    when (1 /= (length grtests)) $ error . unpack $
        "Invalid duplicated group, label: [" <> grlabel <> "]"
    let gr = grtests ! 0
    case gr of
        (TestLabel _ (TestList li)) -> do
            let filtered = (filter (labelFilter tslabel) (fromList li))
            when (0 == (length filtered)) $ error . unpack $
                "Test not found, label: [" <> tslabel <> "]"
            hunitRun filtered
        _ -> error . unpack $
               "Invalid test group,"
            <> " label: [" <> grlabel <>"]"
            <> " paths: [" <> textShow (testCasePaths gr) <> "]"

-- | Runs all, group or one of specified @HUnit@ tests depending on the command line arguments
--
-- Example specifying argument to @stack test@ invocation:
--
-- >
-- > stack test --ta "GroupName testName"
-- >
--
-- If no arguments are specified - all test are run.grlabel
--
-- If single argument is specified - it is interpreted as a name of the group.grlabel
--
-- If two argument are specified - first one is interpreted as a group name, and second one as a test name
--
-- Throws an error on invalid command line argument
--
-- Tests results are printed to stdout
--
-- Arguments:
--
--    * @tests :: Vector Test@: HUnit tests to run
--
hunitMain :: Vector Test -> IO ()
hunitMain tests = do
    args <- (fmap pack) <$> fromList <$> getArgs
    case (length args) of
        0 -> hunitRun tests
        1 -> hunitRunGroup tests (args ! 0)
        2 -> hunitRunSingle tests (args ! 0) (args ! 1)
        _ -> error "Invalid test arguments, expected: [stack test [--ta \"group_name [test_name]\"]"
    return ()

data ExpectedExceptionNotThrown = ExpectedExceptionNotThrown Text
    deriving Show
instance Exception ExpectedExceptionNotThrown

hunitCatchException :: Exception e => Text -> IO a -> IO e
hunitCatchException msg action = do
    outcome <- try $ action
    case outcome of
        Right _ -> throwIO $ ExpectedExceptionNotThrown msg
        Left exc -> return exc
