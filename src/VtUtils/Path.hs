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

module VtUtils.Path
    ( pathIsAbsolute
    , pathConcat
    , pathPrepend
    ) where

import Prelude (Bool(..), (.), (==), (&&), error, otherwise)
import Data.Char (isAlphaNum)
import Data.Monoid ((<>))
import Data.Text (Text, head, index, last, length, unpack)

pathIsAbsolute :: Text -> Bool
pathIsAbsolute path
    | 0 == length path = False
    | '/' == head path = True
    | 1 == length path = False
    | isAlphaNum (head path) && ':' == (index path 1) = True
    | otherwise = False

pathConcat :: Text -> Text -> Text
pathConcat prefix postfix
    | pathIsAbsolute postfix = (error . unpack)
        (  "Invalid path concatenation with abs postfix,"
        <> " prefix: [" <> prefix <>"]"
        <> " postfix: [" <> postfix <>"]")
    | 0 == length prefix = postfix
    | 0 == length postfix = prefix
    | '/' == last prefix = prefix <> postfix
    | otherwise = prefix <> "/" <> postfix

pathPrepend :: Text -> Text -> Text
pathPrepend prefix path =
    case pathIsAbsolute path of
        True -> path
        False -> pathConcat prefix path

