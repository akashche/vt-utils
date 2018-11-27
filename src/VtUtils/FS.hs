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
-- Filesystem utilities

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module VtUtils.FS
    ( fsCopyDirectory
    ) where
import Prelude (IO, (.), ($), (<$>), error, fmap)
import Control.Monad (forM_, unless, when)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import System.Directory (copyFile, createDirectory, doesDirectoryExist, listDirectory)

import VtUtils.Path

-- | Copies a directory recursively
--
-- Throws an exception if source directory does not exist
-- or if destination path already exists
--
-- Arguments:
--
--    * @src :: Text@: Source directory
--    * @dest :: Text@: Destination path
--
fsCopyDirectory :: Text -> Text -> IO ()
fsCopyDirectory src dest = do
    srcex <- doesDirectoryExist (unpack src)
    unless (srcex)
        ((error . unpack) ("Source directory does not exist, src: [" <> src <> "]"))
    destex <- doesDirectoryExist (unpack dest)
    when (destex)
        ((error . unpack) ("Dest directory already exists, dest: [" <> dest <> "]"))
    createDirectory (unpack dest)
    children <- (fmap pack) <$> listDirectory (unpack src)
    forM_ children $ \child -> do
        let srcpath = pathConcat src child
        let destpath = pathConcat dest child
        isdir <- doesDirectoryExist (unpack srcpath)
        if isdir then
            fsCopyDirectory srcpath destpath
        else
            copyFile (unpack srcpath) (unpack destpath)
