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

module FSTest ( fsTest ) where

import Test.HUnit
import Prelude (($), (<$>), fmap, return)
import Control.Exception (bracket_)
import Data.Text (pack)
import Data.Text.IO (readFile)
import Data.Vector (fromList, length)
import System.Directory (createDirectory, listDirectory, removeDirectoryRecursive)

import VtUtils.FS
import VtUtils.HUnit

testCopyDirectory :: Test
testCopyDirectory = TestLabel "testCopyDirectory" $ TestCase $ do
    bracket_
        (createDirectory "test/scratch")
        (removeDirectoryRecursive "test/scratch")
        $ do
            -- success
            _ <- fsCopyDirectory "test/data" "test/scratch/data"
            li <- (fmap pack) <$> fromList <$> listDirectory "test/scratch/data"
            assertEqual "count" 3 (length li)
            contents <- readFile "test/scratch/data/test.txt"
            assertEqual "contents" "foo" contents
            -- src failure
            esrc <- hunitCatchException "source" $ fsCopyDirectory "SRCFAIL" "test/scratch/data1"
            let FSCopyDirectorySourceException {source} = esrc
            assertEqual "src fail" "SRCFAIL" $ source
            -- dest failure
            edest <- hunitCatchException "dest" $ fsCopyDirectory "test/data" "test/scratch/data"
            let FSCopyDirectoryDestException {dest} = edest
            assertEqual "dest fail" "test/scratch/data" $ dest
            return ()

fsTest :: Test
fsTest = TestLabel "FSTest" $ TestList
    [ testCopyDirectory
    ]
