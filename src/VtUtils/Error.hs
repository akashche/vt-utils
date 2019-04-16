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
-- Error reporting utilities
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module VtUtils.Error
    ( errorShow
    ) where

import Prelude (String, (.), ($), show)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import Data.Typeable (Typeable, typeOf)

-- | Formats error message to be used with @Show@
--
-- Arguments:
--
--    * @exc :: Typeable@: Error record or exception
--    * @msg :: Text@: Error message
--
-- Return value: String containing the type name of error and a message
--
errorShow :: (Typeable e) => e -> Text -> String
errorShow exc msg = unpack $
    ((pack . show . typeOf) exc) <> ": " <> msg
