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
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module JSONTest ( jsonTest ) where

import Test.HUnit
import Prelude (Either(..), Int, IO, ($), return)
import Control.Exception (SomeException)
import Data.Aeson (FromJSON, ToJSON, (.=), genericParseJSON, genericToJSON, object, parseJSON, toJSON)
import Data.Either.Combinators (fromRight')
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)

import VtUtils.HUnit
import VtUtils.JSON
import VtUtils.IO
import VtUtils.Text

data Foo = Foo
    { foo :: Int
    , bar :: Text
    } deriving Generic
instance FromJSON Foo
instance ToJSON Foo

-- jsonUnwrapUnaryOptions

data UnaryData = UnaryData
    { value :: Int
    } deriving (Generic)
instance ToJSON UnaryData
    where toJSON = genericToJSON jsonUnwrapUnaryOptions
instance FromJSON UnaryData
    where parseJSON = genericParseJSON jsonUnwrapUnaryOptions

newtype UnaryNewType = UnaryNewType
    { value :: Int
    } deriving (Generic)
instance ToJSON UnaryNewType
    where toJSON = genericToJSON jsonUnwrapUnaryOptions
instance FromJSON UnaryNewType
    where parseJSON = genericParseJSON jsonUnwrapUnaryOptions

data UnaryHolder = UnaryHolder
    { udf :: UnaryData
    , unf :: UnaryNewType
    } deriving (Generic)
instance ToJSON UnaryHolder
instance FromJSON UnaryHolder

_suppress :: IO ()
_suppress = do
    let ud = UnaryData 42
    let _ = value (ud :: UnaryData)
    let un = UnaryNewType 42
    let _ = value (un :: UnaryNewType)
    let uh = UnaryHolder ud un
    let _ = udf uh
    let _ = unf uh
    return ()

objText :: Text
objText = "{\"foo\":42,\"bar\":\"baz\"}"

objTextPretty :: Text
objTextPretty = "{\n    \"foo\": 42,\n    \"bar\": \"baz\"\n}"

testDecodeFile :: Test
testDecodeFile = TestLabel "testDecodeFile" $ TestCase $ do
    -- success
    obj <- jsonDecodeFile "test/data/test.json" :: IO Foo
    assertEqual "file_foo" 42 (foo obj)
    assertEqual "file_bar" "baz" (bar obj)
    -- fail file not found
    errnf <- hunitCatchException "file not found" $ (jsonDecodeFile "test/data/fail.json" :: IO Foo) :: IO SomeException
    assertBool "file not found" $ Text.isPrefixOf "IOWithFileException" $ textShow errnf
    -- fail invalid json
    errio <- hunitCatchException "invalid json" $ (jsonDecodeFile "test/data/test.txt" :: IO Foo) :: IO IOWithFileException
    let IOWithFileException{exception = errd} = errio
    assertBool "invalid json" $ Text.isPrefixOf "JSONDecodeFileException" $ textShow errd
    return ()

testDecodeText :: Test
testDecodeText = TestLabel "testDecodeText" $ TestCase $ do
    -- success
    let Right (obj :: Foo) = jsonDecodeText objText
    assertEqual "text_foo" 42 (foo obj)
    assertEqual "text_bar" "baz" (bar obj)
    -- fail
    let Left err = jsonDecodeText "fail" :: Either JSONDecodeError Foo
    assertBool "decode fail" $ Text.isPrefixOf "JSONDecodeError" $ textShow err
    return ()

testEncodeText :: Test
testEncodeText = TestLabel "testEncodeText" $ TestCase $ do
    let obj = Foo 42 "baz"
    let encoded = jsonEncodeText obj
    assertEqual "encode" objTextPretty encoded
    return ()

testJsonGet :: Test
testJsonGet = TestLabel "testJsonGet" $ TestCase $ do
    let obj = object
            [ "foo" .= (42 :: Int)
            , "bar" .= ("baz" :: Text)
            ]
    assertEqual "get_foo" 42 (fromRight' $ jsonGet "foo" obj :: Int)
    assertEqual "get_bar" "baz" (fromRight' $ jsonGet "bar" obj :: Text)
    return ()

testJsonUnwrapUnary :: Test
testJsonUnwrapUnary = TestLabel "testJsonUnwrapUnary" $ TestCase $ do
    let uh = UnaryHolder (UnaryData 42) (UnaryNewType 43)
    let encoded = jsonEncodeText uh
    assertEqual "unary" "{\n    \"udf\": 42,\n    \"unf\": 43\n}" encoded
    return ()

jsonTest :: Test
jsonTest = TestLabel "JSONTest" $ TestList
    [ testDecodeFile
    , testDecodeText
    , testEncodeText
    , testJsonGet
    , testJsonUnwrapUnary
    ]

