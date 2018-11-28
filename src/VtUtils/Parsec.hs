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
-- Additional combinators and utilities for @Parsec@ library
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module VtUtils.Parsec
    ( Parser
    -- combinators
    , parsecLineContains
    , parsecLinePrefix
    , parsecLineNoPrefix
    , parsecSkipLines
    , parsecSkipManyTill
    , parsecTry
    , parsecWhitespace
    -- non-combinator utils
    , parsecErrorToText
    , parsecParseFile
    , parsecParseText
    ) where

import Prelude (Either(..), Int, IO, (-), (>), (.), ($), (<$>), error, return)
import Data.List (foldl', intersperse)
import Data.Monoid ((<>))
import Data.Text (Text, isInfixOf, isPrefixOf, pack, stripStart, unpack)
import Data.Text.Lazy (fromChunks, toStrict)
import Data.Text.Lazy.Builder (fromString, fromText, toLazyText)
import Text.Parsec (ParseError, (<|>), char, lookAhead, manyTill, noneOf, oneOf, parse, skipMany, try)
import Text.Parsec.Char (anyChar, string)
import Text.Parsec.Error (Message(..), errorMessages, errorPos, messageString)
import Text.Parsec.Pos (sourceColumn, sourceLine, sourceName)
import Text.Parsec.Text.Lazy (Parser)

import VtUtils.IO
import VtUtils.Text

-- combinators

-- | Finds a line containing a specified substring
--
-- Uses @LF@ as a line separator
--
-- Consumes whitespace after the line found
--
-- Resulting line doesn't contain a line separator
--
-- Arguments:
--
--    * @needle :: Text@: Substring to find
--
-- Return value: Line that contains a specified substring
--
parsecLineContains :: Text -> Parser Text
parsecLineContains needle = do
    line <- pack <$> manyTill (noneOf ['\n']) (char '\n')
    if isInfixOf needle line then do
        parsecWhitespace
        return line
    else
        parsecLineContains needle

-- | Finds a line with a specified prefix
--
-- Uses @LF@ as a line separator
--
-- Whitespace is stripped from the start of each line before checking for prefix
--
-- Consumes whitespace after the line found
--
-- Resulting line doesn't contain a line separator
--
-- Arguments:
--
--    * @prefix :: Text@: Prefix to find
--
-- Return value: Line with the specified prefix
--
parsecLinePrefix :: Text -> Parser Text
parsecLinePrefix prefix = do
    line <- pack <$> manyTill (noneOf ['\n']) (char '\n')
    if isPrefixOf prefix (stripStart line) then do
        parsecWhitespace
        return line
    else
        parsecLinePrefix prefix

-- | Finds a line that does not have a specified prefix
--
-- Uses @LF@ as a line separator
--
-- Whitespace is stripped from the start of each line before checking for prefix
--
-- Consumes whitespace after the line found
--
-- Resulting line doesn't contain a line separator
--
-- Arguments:
--
--    * @prefix :: Text@: Prefix that should be skipped
--
-- Return value: First line that does not have a specified prefix
--
parsecLineNoPrefix :: Text -> Parser Text
parsecLineNoPrefix prefix = do
    line <- pack <$> manyTill (noneOf ['\n']) (char '\n')
    if isPrefixOf prefix (stripStart line) then
        parsecLineNoPrefix prefix
    else do
        parsecWhitespace
        return line

-- | Skips a specified number of lines
--
-- Uses @LF@ as a line separator
--
-- Does not consume additional whitespace after the last line skipped (or between the lines)
--
-- Arguments:
--
--    * @count :: Int@: Number of lines to skip
--
parsecSkipLines :: Int -> Parser ()
parsecSkipLines count =
    if count > 0 then do
        _ <- manyTill (noneOf ['\n']) (char '\n')
        parsecSkipLines (count - 1)
    else do
        return ()

-- | Skips all input until the specified substring is found
--
-- Warning: all look-ahead data is kept in memory
--
-- Arguments:
--
--    * @needle :: Text@: Substring to find
--
-- Return value: First line that does not have a specified prefix
--
parsecSkipManyTill :: Text -> Parser ()
parsecSkipManyTill needle = do
    scan
    return ()
    where
        scan = done <|> recur
        done = do
            _ <- try (lookAhead (string (unpack needle)))
            return ()
        recur = do
            _ <- anyChar
            scan
            return ()

-- | The parser @parsecTry p@ behaves like parser p, except that it pretends
-- that it hasn't consumed any input when an error occurs
--
-- This is a re-export of [Text.Parsec.try](https://hackage.haskell.org/package/parsec-3.1.13.0/docs/Text-Parsec.html#v:try)
-- under a different name to not conflict with [Control.Exception.try](https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Exception.html#v:try)
--
-- Arguments:
--
--    * @parser :: Parser a@: Parser to wrap into @try@
--
-- Return value: Resulting value from the specified parser
--
parsecTry :: Parser a -> Parser a
parsecTry = try

-- | Skips one or more whitespace characters
--
-- Note: Lexemes from [Text.Parsec.Token.TokenParser](https://hackage.haskell.org/package/parsec-3.1.13.0/docs/Text-Parsec-Token.html#v:TokenParser)
-- can be used instead
--
parsecWhitespace :: Parser ()
parsecWhitespace = skipMany (oneOf [' ', '\t', '\n', '\r'])

-- | Formats @ParseError@ into @Text@ string
--
-- Arguments:
--
--    * @err :: ParseError@: @ParseError@ thrown by @Parsec@
--
-- Return value: @Text@ representation of a specified error
--
parsecErrorToText :: ParseError -> Text
parsecErrorToText err =
    toStrict $ toLazyText $
            fromText "ParseError:"
        <>  fromText " file: [" <> fromString (sourceName pos) <> fromText "],"
        <>  fromText " line: [" <> fromText (textShow (sourceLine pos)) <> fromText "],"
        <>  fromText " column: [" <> fromText (textShow (sourceColumn pos)) <> fromText "],"
        <>  fromText " messages: [" <> msg <> "]"
    where
        prefix ms = case ms of
            (SysUnExpect _) -> "unexpected: "
            (UnExpect _) -> "unexpected: "
            (Expect _) -> "expected: "
            (Message _) -> "message: "
        errMsgToBuilder ms = fromText (prefix ms) <> fromString (messageString ms)
        pos = errorPos err
        msgList = errorMessages err
        builderList = errMsgToBuilder <$> msgList
        commaList = intersperse (fromText ", ") builderList
        msg = foldl' (<>) (fromText "") commaList

-- | Lazily reads contents from a specified file and parses it using the specified parser
--
-- File contents are decoded as @UTF-8@
--
-- Throws an error on file IO error or parsing error
--
-- Arguments:
--
--    * @parser :: Parser a@: Parser to use for the contents of the file
--    * @path :: ParseError@: Path to a file to parse
--
-- Return value: Resulting value from the specified parser
--
parsecParseFile :: Parser a -> Text -> IO a
parsecParseFile parser path =
    ioWithFileText path $ \tx ->
        case parse parser (unpack path) tx of
            Left err -> (error . unpack) (parsecErrorToText err)
            Right res -> return res

-- | Parser a specified strict @Text@ string using a specified parser
--
-- Note: parser is typed on a lazy @Text@ input (so it can also be used with @parsecParseFile@)
--
-- Throws an error on parsing error
--
-- Arguments:
--
--    * @parser :: Parser a@: Parser to use for the contents of the file
--    * @text :: Text@: @Text@ string to parse
--
-- Return value: Resulting value from the specified parser
--
parsecParseText :: Parser a -> Text -> a
parsecParseText parser text =
    case parse parser "" (fromChunks [text]) of
        Left err -> (error . unpack) (parsecErrorToText err)
        Right res -> res

