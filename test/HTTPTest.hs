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
{-# LANGUAGE Strict #-}

module HTTPTest ( httpTest ) where

import Test.HUnit
import Prelude (Either(..), Int, IO, (.), ($), (==), (/=), (<$>), return)
import Control.Exception (SomeException, try)
import Data.Aeson (Value, (.=), object)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy (toStrict)
import Data.HashMap.Strict (HashMap)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types (status200)
import Network.HTTP.Client (Manager, newManager, parseRequest_, withResponse)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (withApplication)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Network.HTTP.Client as Client

import VtUtils.HTTP
import VtUtils.HUnit
import VtUtils.JSON
import VtUtils.Map
import VtUtils.Text

reqHandler :: Application
reqHandler req respond = do
    (bt, bj) <- if "/json" == httpRequestPath req then do
        outcome <- try $ (httpRequestBodyJSON req :: IO Value)
        case outcome of
            Left (e :: SomeException) -> return (textShow e, object [])
            Right bj -> return ("", bj)
    else do
        bt <- httpRequestBodyText req
        return (bt, object [])
    if "/jsonfail" /= httpRequestPath req then
        respond $ responseLBS status200 [httpContentTypeJSON] $
            encodePretty $ object
                [ "path" .= httpRequestPath req
                , "headers" .= httpRequestHeadersMap req
                , "bodyText" .= bt
                , "bodyJson" .= bj
                ]
    else
        respond $ responseLBS status200 [] $ "json fail"

createManager :: IO Manager
createManager = newManager Client.defaultManagerSettings

testServer :: Test
testServer = TestLabel "testServer" $ TestCase $ do
    man <- createManager
    withApplication (return reqHandler) $ \port ->  do
        -- GET
        let url = "http://127.0.0.1:" <> (textShow port) <> "/"
        let req1 = ((parseRequest_ . unpack) (url <> "foo"))
                { Client.method = "GET"
                , Client.requestHeaders = [("X-Foo", "bar"), ("X-Bar", "42")]
                }
        resp1 <- jsonDecodeTextIO $ (decodeUtf8 . toStrict . Client.responseBody) <$> Client.httpLbs req1 man :: IO Value
        assertEqual "get path" "/foo" $ (jsonGet resp1 "path" :: Text)
        assertEqual "get headers count" 4 $ HashMap.size (jsonGet resp1 "headers" :: HashMap Text Text)
        assertEqual "get headers foo" "bar" $ mapGet (jsonGet resp1 "headers" :: HashMap Text Text) "X-Foo"
        assertEqual "get headers bar" "42" $ mapGet (jsonGet resp1 "headers" :: HashMap Text Text) "X-Bar"
        assertEqual "get body length" 0 $ Text.length (jsonGet resp1 "bodyText" :: Text)
        -- POST text
        let req2 = ((parseRequest_ . unpack) (url <> "foo"))
                { Client.method = "POST"
                , Client.requestBody = Client.RequestBodyBS "bar"
                }
        resp2 <- jsonDecodeTextIO $ (decodeUtf8 . toStrict . Client.responseBody) <$> Client.httpLbs req2 man :: IO Value
        assertEqual "post body text" "bar" $ (jsonGet resp2 "bodyText" :: Text)
        -- POST JSON
        let req3 = ((parseRequest_ . unpack) (url <> "json"))
                { Client.method = "POST"
                , Client.requestBody = (Client.RequestBodyLBS . encodePretty) $ object
                    [ "foo" .= (42 :: Int)
                    , "bar" .= ("baz" :: Text)
                    ]
                }
        resp3 <- jsonDecodeTextIO $ (decodeUtf8 . toStrict . Client.responseBody) <$> Client.httpLbs req3 man :: IO Value
        assertEqual "post text length" 0 $ Text.length (jsonGet resp3 "bodyText" :: Text)
        assertEqual "post json" 42 $ (jsonGet (jsonGet resp3 "bodyJson") "foo" :: Int)
        -- POST invalid JSON
        let req4 = ((parseRequest_ . unpack) (url <> "json"))
                { Client.method = "POST"
                , Client.requestBody = Client.RequestBodyLBS "json fail"
                }
        resp4 <- jsonDecodeTextIO $ (decodeUtf8 . toStrict . Client.responseBody) <$> Client.httpLbs req4 man :: IO Value
        assertBool "json err msg" $ Text.isPrefixOf "HTTPRequestBodyJSONException" (jsonGet resp4 "bodyText")
    return ()

testClient :: Test
testClient = TestLabel "testClient" $ TestCase $ do
    man <- createManager
    withApplication (return reqHandler) $ \port ->  do
        let url = "http://127.0.0.1:" <> (textShow port) <> "/"
        let req = ((parseRequest_ . unpack) (url <> "json"))
                { Client.method = "POST"
                , Client.requestBody = (Client.RequestBodyLBS . encodePretty) $ object
                    [ "foo" .= (42 :: Int)
                    , "bar" .= ("baz" :: Text)
                    ]
                }
        -- receive JSON
        json <- withResponse req man $ \resp ->
            httpResponseBodyJSON url resp 1024 :: IO Value
        assertEqual "json" 42 $ (jsonGet (jsonGet json "bodyJson") "foo" :: Int)
        -- response threshold
        err <- try $
            withResponse req man $ \resp ->
                httpResponseBodyJSON url resp 7 :: IO Value
        case err of
            Right _ -> assertFailure "Response length check failed"
            Left (e :: SomeException) ->
                assertBool "read err" $ Text.isPrefixOf "HTTPResponseBodyException" (textShow e)
        -- headers
        headers <- withResponse req man $ \resp ->
                return (httpResponseHeadersMap resp)
        assertEqual "header" "application/json" $ mapGet headers "Content-Type"
        -- invalid json
        let reqjf = ((parseRequest_ . unpack) (url <> "jsonfail"))
                { Client.method = "POST"
                , Client.requestBody = Client.RequestBodyLBS "req json fail"
                }
        ejf <- withResponse reqjf man $ \resp ->
            hunitCatchException "json fail" $ (httpResponseBodyJSON url resp 1024 :: IO Value)
        let HTTPResponseBodyJSONException {label} = ejf
        assertEqual "json resp err" url $ label
    return ()

httpTest :: Test
httpTest = TestLabel "HTTPTest" $ TestList
    [ testServer
    , testClient
    ]
