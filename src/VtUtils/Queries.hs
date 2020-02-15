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
-- SQL queries files utilities
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module VtUtils.Queries
    ( Queries
    , queriesLoad
    ) where

import Prelude (IO, (==), (>>), return)
import Data.HashMap.Strict (HashMap, fromList)
import Data.Text (Text, dropWhileEnd,pack)
import Text.Parsec ((<|>), alphaNum, anyChar, eof, lookAhead, many1, manyTill, string)
import Text.Parsec.Text.Lazy (Parser)

import VtUtils.Parsec

type Queries = HashMap Text Text

singleQuery :: Parser (Text, Text)
singleQuery = do
    _ <- string "/**"
    parsecWhitespace
    name <- many1 alphaNum
    parsecWhitespace
    _ <- string "*/"
    parsecWhitespace
    value <- manyTill anyChar
        (   (parsecTry (lookAhead (string "/**")) >> return ())
        <|> eof
        )
    return ((convert name), (convert value))
    where
        convert st = dropWhileEnd isEol (pack st)
        isEol ch = '\n' == ch

queries :: Parser Queries
queries = do
    _ <- parsecLineNoPrefix "--"
    parsecSkipManyTill "/**"
    li <- many1 singleQuery
    return (fromList li)

-- | Parses a specified SQL file into a @HashMap@ that contains
-- all SQL queries as map entries
--
-- SQL file example:
--
-- >
-- >   --
-- >   -- test queries
-- >   --
-- >
-- >   /** selectFoo */
-- >   select foo
-- >   from bar
-- >
-- >   /** updateBar */
-- >   update bar
-- >   set foo = 42
-- >
--
-- Note: there must be an empty line after the initial comment lines on the top of the file
--
-- Throws an error on file IO error or parsing error
--
-- Arguments:
--
--    * @path :: Text@: Path to SQL file
--
-- Return value: @HashMap@ containing SQL queries from a file
--
queriesLoad :: Text -> IO Queries
queriesLoad path = do
    qrs <- parsecParseFile queries path
    return qrs

