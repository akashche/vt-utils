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
    parsecSkipOne (string "/**")
    name <- many1 alphaNum
    parsecWhitespace
    parsecSkipOne (string "*/")
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
    parsecSkipLinesPrefix "--"
    parsecSkipManyTill "/**"
    li <- many1 singleQuery
    return (fromList li)

queriesLoad :: Text -> IO Queries
queriesLoad path = do
    qrs <- parsecParseFile queries path
    return qrs

