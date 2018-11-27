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

module VtUtils.Parsec
    ( parsecFloatAsInt
    , parsecInt
    , parsecLineContains
    , parsecLinePrefix
    , parsecSkipLines
    , parsecSkipLinesPrefix
    , parsecSkipLinesTill
    , parsecSkipManyTill
    , parsecSkipOne
    , parsecTry
    , parsecWhitespace
    -- non-combinator utils
    , parsecErrorToText
    , parsecParseFile
    , parsecParseText
    ) where

import Prelude (Either(..), Int, IO, (+), (-), (*), (>), (.), ($), (>>), (<$>), error, read, return)
import Data.List (foldl', intersperse)
import Data.Monoid ((<>))
import Data.Text (Text, isInfixOf, isPrefixOf, pack, stripStart, unpack)
import Data.Text.Lazy (fromChunks, toStrict)
import Data.Text.Lazy.Builder (fromString, fromText, toLazyText)
import Text.Parsec ( ParseError, (<|>), char, lookAhead, many1, manyTill, noneOf, oneOf, option, parse, skipMany, try)
import Text.Parsec.Char (anyChar, digit, string)
import Text.Parsec.Error (Message(..), errorMessages, errorPos, messageString)
import Text.Parsec.Pos (sourceColumn, sourceLine, sourceName)
import Text.Parsec.Text.Lazy (Parser)

import VtUtils.IO
import VtUtils.Text

-- combinators

parsecInt :: Parser Int
parsecInt = do
    valStr <- many1 digit
    let val = read valStr :: Int
    parsecWhitespace
    return val

parsecFloatAsInt :: Parser Int
parsecFloatAsInt = do
    headString <- many1 digit
    let head = read headString :: Int
    tail <- option
        0
        (do
            parsecSkipOne (char '.')
            tailString <- many1 digit
            let tail = read tailString :: Int
            return tail)
    let res = head * 1000 + tail
    parsecWhitespace
    return res

parsecLineContains :: Text -> Parser Text
parsecLineContains needle = do
    lineSt <- manyTill (noneOf ['\n']) (char '\n')
    let line = (pack lineSt)
    parsecWhitespace
    if isInfixOf needle line then
        return line
    else
        parsecLineContains needle

parsecLinePrefix :: Text -> Parser Text
parsecLinePrefix prefix = do
    lineSt <- manyTill (noneOf ['\n']) (char '\n')
    let line = (pack lineSt)
    if isPrefixOf prefix (stripStart line) then
        parsecWhitespace >> return line
    else
        parsecLinePrefix prefix

parsecSkipOne :: Parser a -> Parser ()
parsecSkipOne acomb = do
    _ <- acomb
    parsecWhitespace
    return ()

-- warning: all look-ahead data is kept in memory
parsecSkipManyTill :: Text -> Parser ()
parsecSkipManyTill end = do
    scan
    parsecWhitespace
    return ()
    where
        scan = done <|> recur
        done = do
            _ <- try (lookAhead (string (unpack end)))
            return ()
        recur = do
            _ <- anyChar
            scan
            return ()

parsecSkipLines :: Int -> Parser ()
parsecSkipLines count =
    if count > 0 then do
        _ <- manyTill (noneOf ['\n']) (char '\n')
        parsecWhitespace
        parsecSkipLines (count - 1)
    else
        return ()

parsecSkipLinesPrefix :: Text -> Parser ()
parsecSkipLinesPrefix prefix = do
    lineSt <- manyTill (noneOf ['\n']) (char '\n')
    let line = (pack lineSt)
    if isPrefixOf prefix (stripStart line) then
        parsecSkipLinesPrefix prefix
    else do
        parsecWhitespace
        return ()

parsecSkipLinesTill :: Text -> Parser ()
parsecSkipLinesTill needle = do
    _ <- parsecLineContains needle
    return ()

parsecTry :: Parser a -> Parser a
parsecTry = try

-- lexeme may be used instead
parsecWhitespace :: Parser ()
parsecWhitespace = skipMany (oneOf [' ', '\t', '\n', '\r'])

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

parsecParseFile :: Parser a -> Text -> IO a
parsecParseFile parser path =
    ioWithFileText path $ \tx ->
        case parse parser (unpack path) tx of
            Left err -> (error . unpack) (parsecErrorToText err)
            Right res -> return res

parsecParseText :: Parser a -> Text -> a
parsecParseText parser text =
    case parse parser "" (fromChunks [text]) of
        Left err -> (error . unpack) (parsecErrorToText err)
        Right res -> res

