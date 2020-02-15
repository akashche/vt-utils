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
-- Process spawning utilities
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module VtUtils.Process
    ( processSpawnAndWait
    ) where

import Prelude (Int, IO, ($), (<$>), return)
import Data.Text (Text, unpack)
import Data.Vector (Vector, toList)
import System.Exit (ExitCode(..))
import System.IO (IOMode(WriteMode), withFile)
import System.Process (CreateProcess(..), StdStream(..), proc, waitForProcess, withCreateProcess)

-- | Spawns a new process and waits for it to exit
--
-- Arguments:
--
--    * @executable :: Text@: Path to executable binary
--    * @args :: Vector Text@: Arguments to pass to executable
--    * @out :: Text@ Path to a file, where std output (both @stdout@ and @stderr@) will be written
--
-- Return value: Process exit code
--
processSpawnAndWait :: Text -> Vector Text -> Text -> IO Int
processSpawnAndWait executable args out = do
    code <- withFile (unpack out) WriteMode $ \ha -> do
        let argsList = toList (unpack <$> args)
        let cpRaw = proc (unpack executable) argsList
        let cp = cpRaw
                { std_in = NoStream
                , std_out = UseHandle ha
                , std_err = UseHandle ha
                }
        withCreateProcess cp $ \_ _ _ ph ->
            waitForProcess ph
    case code of
        ExitSuccess -> return 0
        ExitFailure num -> return num
