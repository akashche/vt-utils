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
-- Utilities to work with FS paths
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module VtUtils.Path
    ( pathIsAbsolute
    , pathConcat
    , pathPrepend
    ) where

import Prelude (Bool(..), (==), (&&), otherwise)
import qualified Data.Char as Char
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text

-- | Checks whether specified path is absolute
--
-- Only checks the path string itself, doesn't use FS API.
--
-- Arguments:
--
--    * @path :: Text@: FS path to check
--
-- Return value: @True@ if path is absolute, @False@ otherwise
--
pathIsAbsolute :: Text -> Bool
pathIsAbsolute path
    | 0 == Text.length path = False
    | '/' == Text.head path = True
    | 1 == Text.length path = False
    | Char.isAlphaNum (Text.head path) && ':' == (Text.index path 1) = True
    | otherwise = False

-- | Concatenates two paths
--
-- Specified postfix must be non-absolute
--
-- Arguments:
--
--    * @prefix :: Text@: Path prefix, may be absolute
--    * @postfix :: Text@: Path postfix, must not be absolute
--
-- Return value: Concatenated path
--
pathConcat :: Text -> Text -> Text
pathConcat prefix postfix
    | 0 == Text.length prefix = postfix
    | 0 == Text.length postfix = prefix
    | '/' == Text.last prefix = prefix <> postfix
    | otherwise = prefix <> "/" <> postfix

-- | Prepends an absolute prefix to the relative path
--
-- Does nothing, if path is already absolute
--
-- Arguments:
--
--    * @prefix :: Text@: Path prefix, must be absolute
--    * @postfix :: Text@: Path postfix, may be absolute
--
-- Return value: Path with a specified prefix prepended, if path is relative,
--               specified path unchanged otherwise
--
pathPrepend :: Text -> Text -> Text
pathPrepend prefix path =
    if pathIsAbsolute path then
        path
    else
        pathConcat prefix path

