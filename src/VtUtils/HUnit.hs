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

module VtUtils.HUnit
    ( hunitMain
    , hunitRun
    , hunitRunGroup
    , hunitRunSingle
    ) where

import Prelude (Bool, IO, (==), (/=), (.), (<$>), error, return)
import Control.Monad (when)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import Data.Vector (Vector, (!), filter, fromList, length, toList)
import Test.HUnit (Test(..), runTestTT, testCasePaths)
import System.Environment (getArgs)

import VtUtils.Text (textShow, textVector)

labelFilter :: Text -> Test -> Bool
labelFilter grlabel gr  =
    case gr of
        (TestLabel label _) -> ((pack label) == grlabel)
        _ -> (error . unpack) ("Invalid test group,"
            <> " label: [" <> grlabel <>"]"
            <> " paths: [" <> textShow (testCasePaths gr) <> "]")

hunitRun :: Vector Test -> IO ()
hunitRun tests = do
    _ <- runTestTT (TestList (toList tests))
    return ()

hunitRunGroup :: Vector Test -> Text -> IO ()
hunitRunGroup tests grlabel = do
    let grtests = (filter (labelFilter grlabel) tests)
    when (0 == (length grtests))
        ((error . unpack) ("Test group not found, label: [" <> grlabel <> "]"))
    hunitRun grtests

hunitRunSingle :: Vector Test -> Text -> Text -> IO ()
hunitRunSingle tests grlabel tslabel = do
    let grtests = (filter (labelFilter grlabel) tests)
    when (0 == (length grtests))
        ((error . unpack) ("Test group not found, label: [" <> grlabel <> "]"))
    when (1 /= (length grtests))
        ((error . unpack) ("Invalid duplicated group, label: [" <> grlabel <> "]"))
    let gr = grtests ! 0
    case gr of
        (TestLabel _ (TestList li)) -> do
            let filtered = (filter (labelFilter tslabel) (fromList li))
            when (0 == (length filtered))
                ((error . unpack) ("Test not found, label: [" <> tslabel <> "]"))
            hunitRun filtered
        _ -> (error . unpack) ("Invalid test group,"
            <> " label: [" <> grlabel <>"]"
            <> " paths: [" <> textShow (testCasePaths gr) <> "]")

hunitMain :: Vector Test -> IO()
hunitMain tests = do
    args <- textVector <$> getArgs
    case (length args) of
        0 -> hunitRun tests
        1 -> hunitRunGroup tests (args ! 0)
        2 -> hunitRunSingle tests (args ! 0) (args ! 1)
        _ -> error "Invalid test arguments, expected: [stack test [--ta \"group_name [test_name]\"]"
    return ()
