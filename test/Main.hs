{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.ByteString  (ByteString)
import Data.RESP3       (reply, RespReply(..))
import Data.Text        (Text)
import Scanner
import Test.Tasty
import Test.Tasty.HUnit

testStrSucceeds :: ByteString -> Text -> Assertion
testStrSucceeds bs expected = scanOnly reply bs @?= Right (RespString expected)

testBlobSucceeds :: ByteString -> ByteString -> Assertion
testBlobSucceeds bs expected = scanOnly reply bs @?= Right (RespBlob expected)

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ testCase "Simple String" $ testStrSucceeds "+Test me\r\n" "Test me"
  , testCase "Blob" $ testBlobSucceeds "$7\r\nTest me\r\n" "Test me"
  ]
