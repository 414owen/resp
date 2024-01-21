{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.ByteString                 (ByteString)
import Data.ByteString.Lazy            (LazyByteString)
import Data.RESP3                      (reply, RespReply(..))
import qualified Data.Text.Encoding    as T
import qualified Data.Text             as T
import Data.Text                       (Text)
import qualified Data.ByteString.Char8 as BS8
import Scanner
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck           (testProperty, Arbitrary)

scanReply :: ByteString -> Either String RespReply
scanReply = scanOnly reply

testStr :: ByteString -> Text -> Assertion
testStr bs expected = scanReply bs @?= Right (RespString expected)

testStreamingBlob :: ByteString -> LazyByteString -> Assertion
testStreamingBlob bs expected = scanReply bs @?= Right (RespStreamingBlob expected)

testArray :: ByteString -> [RespReply] -> Assertion
testArray bs expected = scanReply bs @?= Right (RespArray expected)

testDouble :: ByteString -> Double -> Assertion
testDouble bs d = scanReply bs @?= Right (RespDouble d)

testDouble' :: ByteString -> (Double -> Assertion) -> Assertion
testDouble' bs f = case scanReply bs of
  Right (RespDouble d) -> f d
  _ -> assertFailure "Expected to parse into a double"

blobProperties :: ByteString -> String -> (ByteString -> RespReply) -> TestTree
blobProperties leader prefix constr = testProperty "quickcheck" $ \str -> let bs = T.encodeUtf8 (T.pack $ prefix <> str) in
  scanReply (leader <> BS8.pack (show $ BS8.length bs) <> "\r\n" <> bs <> "\r\n") == Right (constr $ BS8.drop (length prefix) bs)

blobTestCases :: ByteString -> (ByteString -> RespReply) -> [TestTree]
blobTestCases leader constr =
  [ testCase "empty" $ scanReply (leader <> "0\r\n\r\n") @?= Right (constr "")
  , testCase "simple" $ scanReply (leader <> "7\r\ntest me\r\n") @?= Right (constr "test me")
  , testCase "multiline" $ scanReply (leader <> "15\r\ntest me\r\nline 2\r\n") @?= Right (constr "test me\r\nline 2")
  , testCase "unicode" $ scanReply (leader <> "11\r\n( ͡° ͜ʖ ͡°)\r\n") @?= Right (constr "( ͡° ͜ʖ ͡°)")
  , testCase "not enough bytes" $ scanReply (leader <> "10\r\nhello\r\n") @?= Left "No more input"
  , testCase "too many bytes" $ scanReply (leader <> "2\r\nhello\r\n") @?= Left "Expected '\\r', but got 'l'"
  , blobProperties leader "" constr
  ]

integerTestCases :: (Arbitrary a, Num a, Show a) => ByteString -> (a -> RespReply) -> [TestTree]
integerTestCases prefix constr =
  -- We currently parse a zero-digit integer as 0,
  -- even though it's technically an invalid response
  -- in the spec. Sometimes being lenient is efficient.
  [ testCase "empty" $ scanReply (prefix <> "\r\n") @?= Left "No more input"
  , testCase "zero" $ scanReply (prefix <> "0\r\n") @?= Right (constr 0)
  , testCase "one" $ scanReply (prefix <> "1\r\n") @?= Right (constr 1)
  , testCase "forty-two" $ scanReply (prefix <> "42\r\n") @?= Right (constr 42)
  , testCase "forty-two" $ scanReply (prefix <> "-42\r\n") @?= Right (constr (-42))
  , testProperty "quickcheck" $ \i -> scanReply (prefix <> BS8.pack (show i) <> "\r\n") == Right (constr i)
  ]

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ testGroup "simple string"
    [ testCase "empty string" $ testStr "+\r\n" ""
    , testCase "nonempty string" $ testStr "+test me\r\n" "test me"
    ]

  , testGroup "simple blobs" $ blobTestCases "$" RespBlob
  , testGroup "blob errors" $ blobTestCases "!" RespBlobError

  , testGroup "verbatim strings"
    [ testGroup "text" $ pure $ blobProperties "=" "txt:" (RespVerbatimString . T.decodeUtf8)
    , testGroup "markdown" $ pure $ blobProperties "=" "mkd:" (RespVerbatimMarkdown . T.decodeUtf8)
    ]

  , testGroup "streaming blob parts"
    [ testCase "empty" $ testStreamingBlob "$?\r\n;0\r\n\r\n" ""
    , testCase "one-part" $ testStreamingBlob "$?\r\n;3\r\nwow\r\n;0\r\n" "wow"
    , testCase "two-part" $ testStreamingBlob "$?\r\n;4\r\nhell\r\n;7\r\no world\r\n;0\r\n" "hello world"
    , testCase "three-part" $ testStreamingBlob "$?\r\n;4\r\nhell\r\n;3\r\no w\r\n;4\r\norld\r\n;0\r\n" "hello world"
    ]

  , testGroup "integer" $ integerTestCases ":" RespInteger
  , testGroup "big integer" $ integerTestCases "(" RespBigInteger

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

    -- from website
    , testCase "nested"
      $ testArray
        "*2\r\n*3\r\n:1\r\n:2\r\n:3\r\n*2\r\n+Hello\r\n-World\r\n"
        $ RespArray <$> [RespInteger <$> [1..3], [RespString "Hello", RespStringError "World"]]

    -- from markdown
    , testCase "nested 2"
      $ testArray
        "*2\r\n*3\r\n:1\r\n$5\r\nhello\r\n:2\r\n#f\r\n"
        [RespArray [RespInteger 1, RespBlob "hello", RespInteger 2], RespBool False]


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

  , testGroup "map"
    [ testCase "empty" $ scanReply "%0\r\n" @?= Right (RespMap [])
    , testCase "simple" $ scanReply "%2\r\n+first\r\n:1\r\n+second\r\n:2\r\n"
        @?= Right (RespMap [(RespString "first", RespInteger 1), (RespString "second", RespInteger 2)])

    , testGroup "streamed"
      [ testCase "empty" $ scanReply "%?\r\n.\r\n" @?= Right (RespMap [])
      , testCase "streamed" $ scanReply "%?\r\n+a\r\n:1\r\n+b\r\n:2\r\n.\r\n"
          @?= Right (RespMap [(RespString "a", RespInteger 1), (RespString "b", RespInteger 2)])
      ]
    ]
  , testGroup "set"
    [ testCase "empty" $ scanReply "~0\r\n" @?= Right (RespSet [])
    , testCase "nonempty" $ scanReply "~5\r\n+orange\r\n+apple\r\n#t\r\n:100\r\n:999\r\n"
        @?= Right (RespSet [RespString "orange", RespString "apple", RespBool True, RespInteger 100, RespInteger 999])
    , testGroup "streamed"
      [ testCase "empty" $ scanReply "~?\r\n.\r\n" @?= Right (RespSet [])
      , testCase "streamed" $ scanReply "~?\r\n+a\r\n:1\r\n+b\r\n:2\r\n.\r\n"
          @?= Right (RespSet [RespString "a", RespInteger 1, RespString "b", RespInteger 2])
      ]
    ]

  , testGroup "attribute"
    [ testCase "empty" $ scanReply "~0\r\n" @?= Right (RespSet [])
    , testCase "nonempty" $ scanReply "|1\r\n+key-popularity\r\n%2\r\n$1\r\na\r\n,0.1923\r\n$1\r\nb\r\n,0.0012\r\n*2\r\n:2039123\r\n:9543892\r\n"
        @?= Right (
          RespAttribute
            [ ( RespString "key-popularity"
              , RespMap
                [ ( RespBlob "a"
                  , RespDouble 0.1923
                  )
                , ( RespBlob "b"
                  , RespDouble 0.0012
                  )
                ]
              )
            ]
          (RespArray
            [ RespInteger 2039123
            , RespInteger 9543892
            ])
        )
    ]
  ]
