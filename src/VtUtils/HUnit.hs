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
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module VtUtils.HUnit
    ( HUnitGroupLabelNotSpecifiedException
    , HUnitDuplicateGroupLabelException
    , HUnitLabelNotFoundException
    , HUnitNonListGroupException
    , HUnitExpectedExceptionNotThrown
    , hunitCatchException
    , HUnitMainException
    , hunitMain
    , hunitRun
    , hunitRunGroup
    , hunitRunSingle
    ) where

import Prelude (Either(..), Int, IO, Maybe(..), Show(..), (+), ($), (>=), (<$>), fmap, otherwise, return)
import Control.Exception (Exception, throwIO, try)
import Data.HashMap.Strict (lookup)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Data.Vector (Vector, (!), fromList, length, toList, singleton)
import qualified Data.Vector as Vector
import Test.HUnit (Test(..), runTestTT, testCasePaths)
import System.Environment (getArgs)

import VtUtils.Error (errorShow)
import VtUtils.Map (mapFromVector)
import VtUtils.Text (textShow)
import VtUtils.Vector (vectorFirstDuplicate)

findNonLabeled :: Vector Test -> Maybe Int
findNonLabeled vec =
    fun 0
    where
        fun idx
            | idx >= Vector.length vec = Nothing
            | otherwise =
                let el = vec ! idx in
                    case el of
                        (TestLabel _ _) -> fun (idx + 1)
                        _ -> Just idx

labelKeyFun :: Int -> Test -> Text
labelKeyFun _ ts =
    case ts of
        (TestLabel lab _) -> pack lab
        (TestCase _) -> "NO_LABEL"
        (TestList _) -> "NO_LABEL"

-- | Exception which indicates that specified test group is invalid
--
data HUnitGroupLabelNotSpecifiedException = HUnitGroupLabelNotSpecifiedException
    { groupIdx :: Int -- ^ Test group index in specified list
    , group :: Test -- ^ Test group
    }
instance Exception HUnitGroupLabelNotSpecifiedException
instance Show HUnitGroupLabelNotSpecifiedException where
    show e@(HUnitGroupLabelNotSpecifiedException {groupIdx, group}) = errorShow e $
               "Invalid test group,"
            <> " index: [" <> textShow groupIdx <> "],"
            <> " paths: [" <> textShow (testCasePaths group) <> "]"

-- | Exception thrown on the duplicate group label
--
data HUnitDuplicateGroupLabelException = HUnitDuplicateGroupLabelException
    { label :: Text -- ^ Duplicate label
    , group1Idx :: Int -- ^ Test group 1 index
    , group1 :: Test -- ^ Test group 1
    , group2Idx :: Int -- ^ Test group 2 index
    , group2 :: Test -- ^ Test group 2
    }
instance Exception HUnitDuplicateGroupLabelException
instance Show HUnitDuplicateGroupLabelException where
    show e@(HUnitDuplicateGroupLabelException {label, group1Idx, group1, group2Idx, group2}) = errorShow e $
               "Invalid duplicate test label specified,"
            <> " label: [" <> label <>"],"
            <> " group1 index: [" <> textShow group1Idx <> "],"
            <> " group1: [" <> textShow (testCasePaths group1) <> "],"
            <> " group2 index: [" <> textShow group2Idx <> "],"
            <> " group2: [" <> textShow (testCasePaths group2) <> "]"

-- | Exception thrown if specified label not found
--
data HUnitLabelNotFoundException = HUnitLabelNotFoundException
    { label :: Text -- ^ Label
    }
instance Exception HUnitLabelNotFoundException
instance Show HUnitLabelNotFoundException where
    show e@(HUnitLabelNotFoundException {label}) = errorShow e $
            "Test group not found, label: [" <> label <> "]"

-- | Exception thrown if specified group does not contain a list of tests
--
data HUnitNonListGroupException = HUnitNonListGroupException
    { label :: Text -- ^ Group label
    , group :: Test -- ^ Invalid group
    }
instance Exception HUnitNonListGroupException
instance Show HUnitNonListGroupException where
    show e@(HUnitNonListGroupException {label, group}) = errorShow e $
               "Group does not contain a list of tests,"
            <> " label: [" <> label <> "],"
            <> " group: [" <> textShow (testCasePaths group) <> "]"

findLabeledTest :: Vector Test -> Text -> IO Test
findLabeledTest tests label = do
    case findNonLabeled tests of
        Just idx -> throwIO $ HUnitGroupLabelNotSpecifiedException idx (tests ! idx)
        Nothing -> return ()
    case vectorFirstDuplicate tests labelKeyFun of
        Just (idx1, idx2) -> throwIO $ HUnitDuplicateGroupLabelException
                { label = labelKeyFun idx1 (tests ! idx1)
                , group1Idx = idx1
                , group1 = tests ! idx1
                , group2Idx = idx2
                , group2 = tests ! idx2
                }
        Nothing -> return ()
    case lookup label (mapFromVector tests labelKeyFun) of
        Nothing -> throwIO $ HUnitLabelNotFoundException label
        Just group -> return group

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
-- Throws an exception if no tests in the specified @Vector@ have specified label
-- or if some tests in the specified @Vector@ have no label at all
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
    group <- findLabeledTest tests label
    hunitRun $ singleton group

-- | Runs a single test from a specified @Vector@ of @HUnit@ tests
--
-- Throws an exception if a test with a specified group label and test label is not found
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
    group <- findLabeledTest tests grlabel
    case group of
        (TestLabel _ (TestList li)) -> do
            test <- findLabeledTest (fromList li) tslabel
            hunitRun $ singleton test
        _ -> throwIO $ HUnitNonListGroupException grlabel group

-- | Exception for 'hunitMain' function
--
data HUnitMainException = HUnitMainException
    { numberOfArgs :: Int -- ^ Number of arguments specified
    }
instance Exception HUnitMainException
instance Show HUnitMainException where
    show e@(HUnitMainException {numberOfArgs}) = errorShow e $
               "Invalid test arguments, "
            <> " number: [" <> (textShow numberOfArgs) <> "],"
            <> " expected: [stack test [--ta \"group_name [test_name]\"]"

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
-- Throws an exception on invalid command line argument
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
        n -> throwIO $ HUnitMainException n
    return ()

-- | Exception for 'hunitCatchException' function
--
data HUnitExpectedExceptionNotThrown = HUnitExpectedExceptionNotThrown Text
    deriving Show
instance Exception HUnitExpectedExceptionNotThrown

-- | Runs the specified action and catches and returs exception of the specified type
--
-- Throws an exception, if exception of specified type is not thrown by the specified action
--
-- Arguments:
--
--    * @msg :: Text@: Error message to include with exception that is thrown on error
--    * @action :: IO a@ IO action that should throw the exception
--
-- Return value: exception record thrown by the specified action
--
hunitCatchException :: Exception e => Text -> IO a -> IO e
hunitCatchException msg action = do
    outcome <- try $ action
    case outcome of
        Right _ -> throwIO $ HUnitExpectedExceptionNotThrown msg
        Left exc -> return exc
