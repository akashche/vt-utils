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
-- HashMap utilities
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module VtUtils.Map
    ( mapGet
    ) where

import Prelude (Maybe(..), (.), error)
import Data.HashMap.Strict (HashMap, lookup)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)

-- | Lookups a key in a @HashMap@
--
-- Throws an error if key not found in a specified @HashMap@
--
-- Arguments:
--
--    * @map :: HashMap Text v@: Map with @Text@ keys
--    * @key :: Text@: Key to lookup
--
-- Return value: Map value that corresponds to the specified key
--
mapGet :: HashMap Text v -> Text -> v
-- mapGet :: (Eq k, Hashable k) => HashMap k v -> k -> v
mapGet map key =
    case lookup key map of
        Just res -> res
        Nothing -> (error . unpack) ("Map entry not found, key: [" <> key <> "]")
