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

module FFITest ( ffiTest ) where

import Test.HUnit
import Prelude (Int, IO, (.), ($), (<$>), fromIntegral, return)
import Data.ByteString (packCString)
import Data.Char (chr)
import Data.Text (pack)
import Data.Text.Encoding (decodeUtf8)
import Data.Word (Word16)
import Foreign (Ptr, nullPtr, peek)
import Foreign.Storable (peekByteOff)

import VtUtils.FFI

testWithUTF8 :: Test
testWithUTF8 = TestLabel "testWithUTF8" $ TestCase $ do
    tx <- ffiWithUTF8 "foo" $ \cs ->
        decodeUtf8 <$> packCString cs
    assertEqual "ut8" "foo" tx
    return ()

testWithUTF16 :: Test
testWithUTF16 = TestLabel "testWithUTF16" $ TestCase $ do
    tx <- ffiWithUTF16 "foo" $ \ws -> do
        ch1 <- (chr . fromIntegral) <$> (peekByteOff ws 0 :: IO Word16)
        ch2 <- (chr . fromIntegral) <$> (peekByteOff ws 2 :: IO Word16)
        ch3 <- (chr . fromIntegral) <$> (peekByteOff ws 4 :: IO Word16)
        return $ pack [ch1, ch2, ch3]
    assertEqual "utf16" "foo" tx
    return ()

testWithPtr :: Test
testWithPtr = TestLabel "testWithPtr" $ TestCase $ do
    num <- ffiWithPtr 42 $ \ptr ->
        peek ptr :: IO Int
    assertEqual "ptr" 42 num
    return ()

testWithPtrPtr :: Test
testWithPtrPtr = TestLabel "testWithPtrPtr" $ TestCase $ do
    nl <- ffiWithPtrPtr $ \pptr -> do
        peek pptr :: IO (Ptr Int)
    assertEqual "pptr" nullPtr nl
    return ()

ffiTest :: Test
ffiTest = TestLabel "FFITest" $ TestList
    [ testWithUTF8
    , testWithUTF16
    , testWithPtr
    , testWithPtrPtr
    ]

