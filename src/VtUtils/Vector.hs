--
-- Copyright 2019, akashche at redhat.com
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
-- Vector utilities
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module VtUtils.Vector
    ( vectorFirstDuplicate
    ) where

import Prelude (Eq, Int, Maybe(..), (+), (>=), otherwise)
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HashMap
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector

-- | Finds the fist two elements which have the same key value
-- for the specified key function
--
-- Arguments:
--
--    * @vec :: Vector a@: Vector to search duplicate in
--    * @keyfun :: (Int -> a -> k)@ Key function, takes index and element as an input
--
-- Return value: two indices containing the duplicate elements, @Nothing@ if no duplicates were found
--
vectorFirstDuplicate :: (Eq k, Hashable k) => Vector a -> (Int -> a -> k) -> Maybe (Int, Int)
vectorFirstDuplicate vec keyfun =
    fun 0 HashMap.empty
    where
--         fun :: Int -> HashMap k Int -> Maybe (Int, Int)
        fun idx map
            | idx >= Vector.length vec = Nothing
            | otherwise =
                let key = keyfun idx (vec ! idx) in
                    case HashMap.lookup key map of
                        Just idx1 -> Just (idx1, idx)
                        Nothing -> fun (idx + 1) (HashMap.insert key idx map)


