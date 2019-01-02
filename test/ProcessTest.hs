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

module ProcessTest ( processTest ) where

import Test.HUnit
import Prelude (return, ($), (>))
import Control.Monad (when)
import Data.Text (length, unpack)
import Data.Text.IO (readFile)
import Data.Vector (fromList)
import System.Directory (removeFile, doesFileExist)

import VtUtils.Process

testSpawnAndWait :: Test
testSpawnAndWait = TestLabel "testSpawnAndWait" $ TestCase $ do
    let ls = "/bin/ls"
    canLs <- doesFileExist (unpack ls)
    when canLs $ do
        let args = fromList ["-l", "-h"]
        let out = "test/ProcessTets_ls_out.txt"
        code <- processSpawnAndWait ls args out
        assertEqual "exit success" 0 code
        outExists <- doesFileExist (unpack out)
        assertBool "output written" outExists
        output <- readFile (unpack out)
        assertBool "output not empty" (length output > 0)
        removeFile (unpack out)
    return ()

processTest :: Test
processTest = TestLabel "ProcessTest" (TestList
    [ testSpawnAndWait
    ])
