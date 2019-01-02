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
-- Foreign Function Interface utilities
-- TODO

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module VtUtils.FFI
    ( ffiWithPtr
    , ffiWithPtrPtr
    , ffiWithUTF8
    , ffiWithUTF16
    ) where

import Prelude (IO, ($), fromIntegral)
import Data.Text (Text)
import Data.Text.Foreign (asForeignPtr, withCStringLen)
import Data.Vector.Storable.Mutable (grow, replicate, unsafeFromForeignPtr0, unsafeWith, write)
import Data.Word (Word16)
import Foreign (Ptr, newForeignPtr_, nullPtr)
import Foreign.C.String (CString)
import Foreign.Storable (Storable)

ffiWithUTF8 :: Text -> (CString -> IO a) -> IO a
ffiWithUTF8 text fun =
    withCStringLen text $ \(ptr, len) -> do
        fptr <- newForeignPtr_ ptr
        let vecInit = unsafeFromForeignPtr0 fptr len
        vec <- grow vecInit 1
        write vec len 0
        unsafeWith vec fun

ffiWithUTF16 :: Text -> (Ptr Word16 -> IO a) -> IO a
ffiWithUTF16 text fun = do
    (fptr, tlen) <- asForeignPtr text
    let len = fromIntegral tlen
    let vecInit = unsafeFromForeignPtr0 fptr len
    vec <- grow vecInit 1
    write vec len 0
    unsafeWith vec fun

ffiWithPtr :: Storable a => a -> (Ptr a -> IO b) -> IO b
ffiWithPtr val fun = do
    vec <- replicate 1 val
    unsafeWith vec fun

ffiWithPtrPtr :: (Ptr (Ptr a) -> IO b) -> IO b
ffiWithPtrPtr fun = do
    vec <- replicate 1 (nullPtr :: Ptr a)
    unsafeWith vec fun
