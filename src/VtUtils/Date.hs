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
-- Date format utilities

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

-- | Formats a date into a Text string using specified formatting string
--
-- Arguments:
--
--    * @format :: Text@: [Format string](https://hackage.haskell.org/package/time-1.9.2/docs/Data-Time-Format.html#v:formatTime)
--    * @dt :: UTCTime@: Date to format
--
-- Return value: String containing a date in a specified format
--
dateFormat :: Text -> UTCTime -> Text
dateFormat format dt =
    pack (formatTime defaultTimeLocale (unpack format) dt)

-- | Formats a date into a Text string using ISO8601 formatting string
--
-- Format: @%Y-%m-%d %H:%M:%S@
--
-- Output example: @2018-11-25 00:00:01@
--
-- Arguments:
--
--    * @dt :: UTCtime@: Date to format
--
-- Return value: String containing a date in ISO8601 format
--
dateFormatISO8601 :: UTCTime -> Text
dateFormatISO8601 dt = dateFormat iso8601 dt

-- | Parses Text string using ISO8601 format
--
-- Raises an [error](http://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:error), if
-- input string cannot be parsed as ISO8601 date
--
-- Expected input example: @2018-11-25 00:00:01@
--
-- Arguments:
--
--    * @tx :: Text@: Text string containing a date in ISO8601 format
--
-- Return value: Parsed date
--
dateParseISO8601 :: Text -> UTCTime
dateParseISO8601 tx =
    case parseTimeM False defaultTimeLocale (unpack iso8601) (unpack tx) :: Maybe UTCTime of
        Just tm -> tm
        Nothing -> (error . unpack) ("Error parsing ISO8601 format, date: [" <> tx <> "]")
