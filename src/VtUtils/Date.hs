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

module VtUtils.Date
    ( dateFormat
    , dateFormatISO8601
    , dateParseISO8601
    ) where

import Prelude (Bool(False), Maybe(..), (.), error)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)

iso8601 :: Text
iso8601 = "%Y-%m-%d %H:%M:%S"

dateFormat :: Text -> UTCTime -> Text
dateFormat format tm =
    pack (formatTime defaultTimeLocale (unpack format) tm)

dateFormatISO8601 :: UTCTime -> Text
dateFormatISO8601 tm = dateFormat iso8601 tm

dateParseISO8601 :: Text -> UTCTime
dateParseISO8601 tx =
    case parseTimeM False defaultTimeLocale (unpack iso8601) (unpack tx) :: Maybe UTCTime of
        Just tm -> tm
        Nothing -> (error . unpack) ("Error parsing ISO8601 format, date: [" <> tx <> "]")
