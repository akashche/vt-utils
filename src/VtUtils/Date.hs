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
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module VtUtils.Date
    ( dateFormat
    , dateFormatISO8601
    , DateParseISO8601Error(..)
    , dateParseISO8601
    ) where

import Prelude (Bool(False), Either(..), Maybe(..), Show(..), ($))
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)

import VtUtils.Error (errorShow)

iso8601 :: Text
iso8601 = "%Y-%m-%d %H:%M:%S"

-- | Formats a date into a Text string using specified formatting string
--
-- Arguments:
--
--    * @format :: Text@: [Format string](https://hackage.haskell.org/package/time-1.9.2/docs/Data-Time-Format.html#v:formatTime)
--    * @date :: UTCTime@: Date to format
--
-- Return value: String containing a date in a specified format
--
dateFormat :: Text -> UTCTime -> Text
dateFormat format date =
    pack $ formatTime defaultTimeLocale (unpack format) date

-- | Formats a date into a Text string using ISO8601 formatting string
--
-- Format: @%Y-%m-%d %H:%M:%S@
--
-- Output example: @2018-11-25 00:00:01@
--
-- Arguments:
--
--    * @date :: UTCTime@: Date to format
--
-- Return value: String containing a date in ISO8601 format
--
dateFormatISO8601 :: UTCTime -> Text
dateFormatISO8601 date = dateFormat iso8601 date

-- | Error record for `dateParseISO8601` function
--
data DateParseISO8601Error = DateParseISO8601Error
    { invalidDate :: Text -- ^ Invalid date string specified for parsing
    }
instance Show DateParseISO8601Error where
    show e@(DateParseISO8601Error {invalidDate}) = errorShow e $
               "Error parsing with ISO8601 format,"
            <> " date: [" <> invalidDate <> "]"

-- | Parses Text string using ISO8601 format
--
-- Expected input example: @2018-11-25 00:00:01@
--
-- Arguments:
--
--    * @text :: Text@: Text string containing a date in ISO8601 format
--
-- Return value: Parsed date or error message
--
dateParseISO8601 :: Text -> Either DateParseISO8601Error UTCTime
dateParseISO8601 text =
    case parseTimeM False defaultTimeLocale (unpack iso8601) (unpack text) :: Maybe UTCTime of
        Just tm -> Right tm
        Nothing -> Left $ DateParseISO8601Error text
