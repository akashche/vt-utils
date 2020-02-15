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
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module VtUtils.Map
    ( mapFromVector
    ) where

import Prelude (Eq, Int)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Vector (Vector, ifoldl')

-- | Creates a @HashMap@ from a @Vector@
--
-- Creates a @HashMap@ and fills it using the @Vector@ elements as
-- values and elements with key function as keys.
--
-- If key function returns duplicate keys for some elements, previous entry is
-- replaced with a new one.
--
-- Arguments:
--
--    * @vec :: Vector v@: Input @Vector@
--    * @keyfun :: (Int -> v -> k)@: Key function that takes an element index and element and returns the map key
--
-- Return value: @HashMap@ filled with elements from input @Vector@
--
mapFromVector :: (Eq k, Hashable k) => Vector v -> (Int -> v -> k) -> HashMap k v
mapFromVector vec keyfun =
    ifoldl' fun HashMap.empty vec
    where
        fun map idx v = HashMap.insert (keyfun idx v) v map

