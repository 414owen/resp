{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

#if !MIN_VERSION_base(4,8,0)
import Data.Functor
import Control.Applicative
#endif

#if !MIN_VERSION_base(4,11,0)
# if MIN_VERSION_base(4,9,0)
import Data.Semigroup
import Data.Monoid (mempty)
# else
import Data.Monoid ((<>), mempty)
# endif
#endif

import Data.ByteString                 (ByteString)
import Data.RESP                       (RespReply(..), RespExpr(..))
import qualified Data.RESP             as R3
import qualified Data.Text.Encoding    as T
import qualified Data.Text             as T
import Data.Text                       (Text)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy  as BSL
import Scanner
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck           (testProperty, Arbitrary)

parseExpr :: ByteString -> Either String RespExpr
parseExpr = scanOnly R3.parseExpression

parseReply :: ByteString -> Either String RespReply
parseReply = scanOnly R3.parseReply

testStr :: ByteString -> Text -> Assertion
testStr bs expected = parseExpr bs @?= Right (RespString expected)

testStreamingBlob :: ByteString -> BSL.ByteString -> Assertion
testStreamingBlob bs expected = parseExpr bs @?= Right (RespStreamingBlob expected)

testArray :: ByteString -> [RespExpr] -> Assertion
testArray bs expected = parseExpr bs @?= Right (RespArray expected)

testDouble :: ByteString -> Double -> Assertion
testDouble bs d = parseExpr bs @?= Right (RespDouble d)

testDouble' :: ByteString -> (Double -> Assertion) -> Assertion
testDouble' bs f = case parseExpr bs of
  Right (RespDouble d) -> f d
  _ -> assertFailure "Expected to parse into a double"

blobProperties :: ByteString -> String -> (ByteString -> RespExpr) -> TestTree
blobProperties leader prefix constr = testProperty "quickcheck" $ \str -> let bs = T.encodeUtf8 (T.pack $ prefix <> str) in
  parseExpr (leader <> BS8.pack (show $ BS8.length bs) <> "\r\n" <> bs <> "\r\n") == Right (constr $ BS8.drop (length prefix) bs)

blobTestCases :: ByteString -> (ByteString -> RespExpr) -> [TestTree]
blobTestCases leader constr =
  [ testCase "empty" $ parseExpr (leader <> "0\r\n\r\n") @?= Right (constr "")
  , testCase "simple" $ parseExpr (leader <> "7\r\ntest me\r\n") @?= Right (constr "test me")
  , testCase "multiline" $ parseExpr (leader <> "15\r\ntest me\r\nline 2\r\n") @?= Right (constr "test me\r\nline 2")
  , testCase "unicode" $ parseExpr (leader <> "11\r\n( ͡° ͜ʖ ͡°)\r\n") @?= Right (constr "( ͡° ͜ʖ ͡°)")
  , testCase "not enough bytes" $ parseExpr (leader <> "10\r\nhello\r\n") @?= Left "No more input"
  , testCase "too many bytes" $ parseExpr (leader <> "2\r\nhello\r\n") @?= Left "Expected '\\r', but got 'l'"
  , blobProperties leader "" constr
  ]

integerTestCases :: (Arbitrary a, Num a, Show a) => ByteString -> (a -> RespExpr) -> [TestTree]
integerTestCases prefix constr =
  -- We currently parse a zero-digit integer as 0,
  -- even though it's technically an invalid response
  -- in the spec. Sometimes being lenient is efficient.
  [ testCase "empty" $ parseExpr (prefix <> "\r\n") @?= Left "No more input"
  , testCase "zero" $ parseExpr (prefix <> "0\r\n") @?= Right (constr 0)
  , testCase "one" $ parseExpr (prefix <> "1\r\n") @?= Right (constr 1)
  , testCase "forty-two" $ parseExpr (prefix <> "42\r\n") @?= Right (constr 42)
  , testCase "forty-two" $ parseExpr (prefix <> "-42\r\n") @?= Right (constr (-42))
  , testProperty "quickcheck" $ \i -> parseExpr (prefix <> BS8.pack (show i) <> "\r\n") == Right (constr i)
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
    [ testCase "RESP2 bulk string null" $ parseExpr "$-1\r\n" @?= Right RespNull
    ]

  , testGroup "array"
    [ testCase "empty" $ parseExpr "*0\r\n" @?= Right (RespArray [])
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
    , testCase "null" $ parseExpr "*-1\r\n" @?= Right RespNull

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

  , testCase "null" $ parseExpr "_\r\n" @?= Right RespNull

  , testGroup "boolean"
    [ testCase "true" $ parseExpr "#t\r\n" @?= Right (RespBool True)
    , testCase "false" $ parseExpr "#f\r\n" @?= Right (RespBool False)
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
    , testProperty "quickcheck" $ \d -> parseExpr ("," <> BS8.pack (show d) <> "\r\n") == Right (RespDouble d)
    ]

  , testGroup "map"
    [ testCase "empty" $ parseExpr "%0\r\n" @?= Right (RespMap [])
    , testCase "simple" $ parseExpr "%2\r\n+first\r\n:1\r\n+second\r\n:2\r\n"
        @?= Right (RespMap [(RespString "first", RespInteger 1), (RespString "second", RespInteger 2)])

    , testGroup "streamed"
      [ testCase "empty" $ parseExpr "%?\r\n.\r\n" @?= Right (RespMap [])
      , testCase "streamed" $ parseExpr "%?\r\n+a\r\n:1\r\n+b\r\n:2\r\n.\r\n"
          @?= Right (RespMap [(RespString "a", RespInteger 1), (RespString "b", RespInteger 2)])
      ]
    ]
  , testGroup "set"
    [ testCase "empty" $ parseExpr "~0\r\n" @?= Right (RespSet [])
    , testCase "nonempty" $ parseExpr "~5\r\n+orange\r\n+apple\r\n#t\r\n:100\r\n:999\r\n"
        @?= Right (RespSet [RespString "orange", RespString "apple", RespBool True, RespInteger 100, RespInteger 999])
    , testGroup "streamed"
      [ testCase "empty" $ parseExpr "~?\r\n.\r\n" @?= Right (RespSet [])
      , testCase "streamed" $ parseExpr "~?\r\n+a\r\n:1\r\n+b\r\n:2\r\n.\r\n"
          @?= Right (RespSet [RespString "a", RespInteger 1, RespString "b", RespInteger 2])
      ]
    ]

  , testGroup "attribute"
    [ testCase "empty" $ parseExpr "~0\r\n" @?= Right (RespSet [])
    -- from markdown spec
    , testCase "nonempty" $ parseExpr "|1\r\n+key-popularity\r\n%2\r\n$1\r\na\r\n,0.1923\r\n$1\r\nb\r\n,0.0012\r\n*2\r\n:2039123\r\n:9543892\r\n"
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

  , testGroup "push"
    -- from markdown spec
    [ testCase "empty" $ parseReply ">1\r\n+test\r\n\r\n" @?= Right (RespPush "test" [])
    , testCase "simple message type" $ parseReply ">3\r\n+message\r\n+somechannel\r\n+this is the message\r\n"
        @?= Right (RespPush "message" [RespString "somechannel", RespString "this is the message"])
    , testCase "blob string els" $ parseReply ">3\r\n$7\r\nmessage\r\n$6\r\nsecond\r\n$5\r\nHello\r\n"
        @?= Right (RespPush "message" [RespBlob "second", RespBlob "Hello"])
    ]
  ]
