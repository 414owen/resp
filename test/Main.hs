{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.ByteString  (ByteString)
import Data.RESP3       (reply, RespReply(..))
import Data.Text        (Text)
import Scanner
import Test.Tasty
import Test.Tasty.HUnit
import Data.ByteString.Lazy (LazyByteString)
import Data.Int (Int64)

import qualified Data.ByteString.Char8 as BS8
import Test.Tasty.QuickCheck (testProperty)

scanReply :: ByteString -> Either String RespReply
scanReply = scanOnly reply

testStr :: ByteString -> Text -> Assertion
testStr bs expected = scanReply bs @?= Right (RespString expected)

testBlob :: ByteString -> ByteString -> Assertion
testBlob bs expected = scanReply bs @?= Right (RespBlob expected)

testStreamingBlob :: ByteString -> LazyByteString -> Assertion
testStreamingBlob bs expected = scanReply bs @?= Right (RespStreamingBlob expected)

testInt :: ByteString -> Int64 -> Assertion
testInt bs expected = scanReply bs @?= Right (RespInteger expected)

testArray :: ByteString -> [RespReply] -> Assertion
testArray bs expected = scanReply bs @?= Right (RespArray expected)

testDouble :: ByteString -> Double -> Assertion
testDouble bs d = scanReply bs @?= Right (RespDouble d)

testDouble' :: ByteString -> (Double -> Assertion) -> Assertion
testDouble' bs f = case scanReply bs of
  Right (RespDouble d) -> f d
  _ -> assertFailure "Expected to parse into a double"

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ testGroup "simple string"
    [ testCase "empty string" $ testStr "+\r\n" ""
    , testCase "nonempty string" $ testStr "+test me\r\n" "test me"
    ]

  , testGroup "simple blobs"
    [ testCase "empty" $ testBlob "$0\r\n\r\n" ""
    , testCase "simple" $ testBlob "$7\r\ntest me\r\n" "test me"
    , testCase "multiline" $ testBlob "$15\r\ntest me\r\nline 2\r\n" "test me\r\nline 2"
    , testCase "unicode" $ testBlob "$11\r\n( ͡° ͜ʖ ͡°)\r\n" "( ͡° ͜ʖ ͡°)"
    , testCase "not enough bytes" $ scanReply "$10\r\nhello\r\n" @?= Left "No more input"
    , testCase "too many bytes" $ scanReply "$2\r\nhello\r\n" @?= Left "Expected '\\r', but got 'l'"
    ]

  , testGroup "streaming blob parts"
    [ testCase "empty" $ testStreamingBlob "$?\r\n;0\r\n\r\n" ""
    , testCase "one-part" $ testStreamingBlob "$?\r\n;3\r\nwow\r\n;0\r\n" "wow"
    , testCase "two-part" $ testStreamingBlob "$?\r\n;4\r\nhell\r\n;7\r\no world\r\n;0\r\n" "hello world"
    , testCase "three-part" $ testStreamingBlob "$?\r\n;4\r\nhell\r\n;3\r\no w\r\n;4\r\norld\r\n;0\r\n" "hello world"
    ]

  , testGroup "integer"
    -- We currently parse a zero-digit integer as 0,
    -- even though it's technically an invalid response
    -- in the spec. Sometimes being lenient is efficient.
    [ testCase "empty" $ scanReply ":\r\n" @?= Left "No more input"
    , testCase "zero" $ testInt ":0\r\n" 0
    , testCase "one" $ testInt ":1\r\n" 1
    , testCase "forty-two" $ testInt ":42\r\n" 42
    , testCase "forty-two" $ testInt ":-42\r\n" (-42)
    ]

  , testGroup "null"
    [ testCase "RESP2 bulk string null" $ scanReply "$-1\r\n" @?= Right RespNull
    ]

  , testGroup "array"
    [ testCase "empty" $ scanReply "*0\r\n" @?= Right (RespArray [])
    , testCase "[hello, world]"
      $ testArray
        "*2\r\n$5\r\nhello\r\n$5\r\nworld\r\n"
        [RespBlob "hello", RespBlob "world"]
    , testCase "[1 .. 3]"
      $ testArray
        "*3\r\n:1\r\n:2\r\n:3\r\n"
        $ RespInteger <$> [1..3]
    , testCase "heterogeneous"
      $ testArray
        "*5\r\n:1\r\n:2\r\n:3\r\n:4\r\n$5\r\nhello\r\n"
        $ map RespInteger [1..4] <> [RespBlob "hello"]

    , testCase "nested"
      $ testArray
        "*2\r\n*3\r\n:1\r\n:2\r\n:3\r\n*2\r\n+Hello\r\n-World\r\n"
        $ RespArray <$> [RespInteger <$> [1..3], [RespString "Hello", RespStringError "World"]]

    -- website: "Null arrays"
    , testCase "null" $ scanReply "*-1\r\n" @?= Right RespNull

    -- website: "Null elements in arrays"
    , testCase "null element"
      $ testArray
        "*3\r\n$5\r\nhello\r\n$-1\r\n$5\r\nworld\r\n"
        [RespBlob "hello", RespNull, RespBlob "world"]

    -- from markdown spec
    , testCase "streaming"
      $ testArray
        "*?\r\n:1\r\n:2\r\n:3\r\n.\r\n"
        $ RespInteger <$> [1..3]

    ]

    , testCase "null" $ scanReply "_\r\n" @?= Right RespNull

    , testGroup "boolean"
      [ testCase "true" $ scanReply "#t\r\n" @?= Right (RespBool True)
      , testCase "false" $ scanReply "#f\r\n" @?= Right (RespBool False)
      ]

    , testGroup "double"
      [ testCase "from int" $ testDouble ",42\r\n" 42
      , testCase "with decimal pt" $ testDouble ",42.12\r\n" 42.12
      , testCase "with exponent" $ testDouble ",42.12e2\r\n" 4212
      , testCase "with positive exponent" $ testDouble ",42.12e+2\r\n" 4212
      , testCase "negative with negative exponent" $ testDouble ",-42.12e-2\r\n" (-0.4212)

      , testCase "inf" $ testDouble' ",inf\r\n" $ assertBool "is infinite" . isInfinite
      , testCase "-inf" $ testDouble' ",-inf\r\n" $ \d -> do
          assertBool "is infinite" $ isInfinite d
          assertBool "== negate (1/0)" $ d == negate (1 / 0)
      , testCase "nan" $ testDouble' ",nan\r\n" $ assertBool "is NaN" . isNaN

      -- Looks like we can also parse all `show`n doubles
      , testProperty "quickcheck" $ \d -> scanReply ("," <> BS8.pack (show d) <> "\r\n") == Right (RespDouble d)

      ]
  ]
