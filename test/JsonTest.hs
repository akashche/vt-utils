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

module JsonTest ( jsonTest ) where

import Test.HUnit
import Prelude (Int, IO, ($), return)
import Data.Aeson (FromJSON, ToJSON, (.=), object)
import Data.Text (Text)
import GHC.Generics (Generic)

import VtUtils.Json

data Foo = Foo
    { foo :: Int
    , bar :: Text
    } deriving Generic
instance FromJSON Foo
instance ToJSON Foo

objText :: Text
objText = "{\"foo\":42,\"bar\":\"baz\"}"

testDecodeFile :: Test
testDecodeFile = TestLabel "testDecodeFile" $ TestCase $ do
    obj <- jsonDecodeFile "test/data/test.json" :: IO Foo
    assertEqual "file_foo" 42 (foo obj)
    assertEqual "file_bar" "baz" (bar obj)
    return ()

testDecodeText :: Test
testDecodeText = TestLabel "testDecodeText" $ TestCase $ do
    let obj = jsonDecodeText objText :: Foo
    assertEqual "text_foo" 42 (foo obj)
    assertEqual "text_bar" "baz" (bar obj)
    return ()

testEncodeText :: Test
testEncodeText = TestLabel "testEncodeText" $ TestCase $ do
    let obj = Foo 42 "baz"
    let encoded = jsonEncodeText obj
    assertEqual "encode" objText encoded
    return ()

testJsonGet :: Test
testJsonGet = TestLabel "testJsonGet" $ TestCase $ do
    let obj = object
            [ "foo" .= (42 :: Int)
            , "bar" .= ("baz" :: Text)
            ]
    assertEqual "get_foo" 42 (jsonGet obj "foo" :: Int)
    assertEqual "get_bar" "baz" (jsonGet obj "bar" :: Text)
    return ()

jsonTest :: Test
jsonTest = TestLabel "JsonTest" (TestList
    [ testDecodeFile
    , testDecodeText
    , testEncodeText
    , testJsonGet
    ])

