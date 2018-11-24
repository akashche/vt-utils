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

module VtUtils.Text
    ( textShow
    , textVector
    ) where

import Prelude (Maybe, Show, String, otherwise, show)
import Data.ByteString (ByteString)
import Data.Maybe (isJust, fromJust)
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import Data.Typeable (Typeable, cast)
import Data.Vector (Vector, fromList, map)

textShow :: (Show a, Typeable a) => a -> Text
textShow val
    | isJust castedText = fromJust castedText
    | isJust castedString = pack (fromJust castedString)
    | isJust castedBytes = decodeUtf8 (fromJust castedBytes)
    | otherwise = pack (show val)
    where
        castedText = cast val :: Maybe Text
        castedString = cast val :: Maybe String
        castedBytes = cast val :: Maybe ByteString

textVector :: [String] -> Vector Text
textVector list =
    map pack (fromList list)
