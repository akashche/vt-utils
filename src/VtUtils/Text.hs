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
-- Text utilities
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module VtUtils.Text
    ( textShow
    , textSplit
    , textFormatParts
    , textFormat
    , textDecodeUtf8
    ) where

import Prelude (Maybe, Show, String, (+), (-), (.), ($), (==), (/=), fst, error, otherwise, show)
import Data.ByteString (ByteString)
import Data.Maybe (isJust, fromJust)
import Data.Monoid ((<>))
import Data.List (reverse)
import Data.Text (Text, breakOnAll, drop, pack, unpack)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (fromText, toLazyText)
import Data.Typeable (Typeable, cast)
import Data.Vector (Vector, (!), fromList, ifoldl', length)

-- | Stringifies specified value
--
-- If input is @Text@, @String@ or @ByteString@, it is returned as a
-- @Text@ string without additional quotes around it
--
-- Arguments:
--
--    * @val :: a@: Value to stringify
--
-- Return value: @Text@ string representation of a specified value
--
textShow :: (Show a, Typeable a) => a -> Text
textShow val
    | isJust castedText = fromJust castedText
    | isJust castedString = pack (fromJust castedString)
    | isJust castedBytes = textDecodeUtf8 (fromJust castedBytes)
    | otherwise = pack (show val)
    where
        castedText = cast val :: Maybe Text
        castedString = cast val :: Maybe String
        castedBytes = cast val :: Maybe ByteString

-- | Splits specified @Text@ string into a @Vector@ of parts using specified delimiter
--
-- Delimiter must be non-empty
--
-- Arguments:
--
--    * @haystack :: Text@: string to split
--    * @needle :: Text@ delimiter
--
-- Return value: @Vector Text@ vector containing the parts of the input string
--
textSplit :: Text -> Text -> Vector Text
textSplit haystack needle =
    (fromList . reverse) $ fst $ ifoldl' fun ([], 0) pairs
    where
        nl = Text.length needle
        empt parts
            | 0 == length parts = fromList [(haystack, "")]
            | otherwise = parts
        pairs = empt $ fromList $ breakOnAll needle haystack
        fun (ac, al) idx (pref, suf)
            | (length pairs) - 1 == idx =
                if 0 /= Text.length suf then
                    (drop nl suf : drop al pref : ac, 0)
                else
                    (drop al pref : ac, 0)
            | 0 == idx = ([pref], (Text.length pref) + nl)
            | otherwise = (drop al pref : ac, (Text.length pref) + nl)

-- | Concatenates specified @Vector@ of string parts interspersing it with specified parameters
--
-- Length of the parameters @Vector@ must be equal to the length of the parts @Vector@ minus 1
--
-- Arguments:
--
--    * @parts :: Vector Text@: string parts
--    * @params :: Vector Text@ parameters to intersperse into the parts
--
-- Return value: @Vector Text@ vector containing the parts of the input string
--
textFormatParts :: Vector Text -> Vector Text -> Text
textFormatParts parts params =
    toStrict $ toLazyText $ ifoldl' fun (fromText "") (check parts)
    where
        check vec =
            if (length vec) == (length params) + 1 then
                vec
            else
                error . unpack $
                       "Invalid format string,"
                    <> " placeholders count: [" <> (textShow ((length vec) - 1)) <> "],"
                    <> " parameters count: [" <> (textShow (length params)) <> "]"
        fun ac idx el
            | length params == idx = ac <> (fromText el)
            | otherwise = ac <> (fromText el) <> (fromText $ params ! idx)

-- | Formats specified template with specified parameters @Vector@
--
-- Template must use @{}@ string for placeholders.
--
-- Number of specified parameters must equal to the number of placeholders inside the template.
--
-- Template preparation is relatively expensive, consider using @textFormatParts@ for
-- frequently used templates.
--
-- Arguments:
--
--    * @template :: Text@: string parts
--    * @params :: Vector Text@ parameters to intersperse into the parts
--
-- Return value: @Vector Text@ vector containing the parts of the input string
--
textFormat :: Text -> Vector Text -> Text
textFormat template params =
    textFormatParts (textSplit template "{}") params

textDecodeUtf8 :: ByteString -> Text
textDecodeUtf8 = decodeUtf8With lenientDecode
