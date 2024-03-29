{-# LANGUAGE CPP               #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE ApplicativeDo     #-}
#endif
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE DeriveGeneric     #-}

{-|
RESP is the wire protocol that Redis uses.
The latest version is RESP3, which at time of writing
seems relatively complete, but may still be evolving.

This module parses the entire RESP3 spec (as of 2024-01-26),
but also parses some invalid RESP forms that Redis may return,
eg `-nan`, and parses RESP2 forms that have been removed from
the spec (eg `$-1\\r\\n`).
-}

module Data.RESP
  ( RespMessage(..)
  , RespExpr(..)
  , parseMessage
  , parseExpression
  ) where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy  as BSL
import qualified Scanner               as Scanner

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

import Data.ByteString      (ByteString)
import Data.Char            (digitToInt)
import Data.Int             (Int64)
import Scanner              (Scanner)
import Control.Monad        (when, replicateM)
import GHC.Generics         (Generic)

#if MIN_VERSION_bytestring(0,10,0)
lazyBsToStrict :: BSL.ByteString -> ByteString
lazyBsToStrict = BSL.toStrict
#else
lazyBsToStrict :: BSL.ByteString -> ByteString
lazyBsToStrict = BS.concat . BSL.toChunks
#endif

-- | A message from the server (eg. Redis) to the client.
-- This can be a push message (for pub/sub), or a reply to a command
-- issued by the client.
data RespMessage
  = RespPush !ByteString ![RespExpr]
  | RespReply !RespExpr
  deriving (Show, Eq, Ord, Generic)

-- | RESP3 Expression.
--
-- This descriminates the difference between RespString and RespBlob,
-- even though both contain bytestrings, in order to not throw away
-- information. A caller might care whether the response was delivered
-- with "+", or "$".
--
-- We do not, however descriminate between the different encodings of
-- null. As far as I can tell, these are considered a mistake in the
-- previous versions of the RESP spec, and clients should treat the
-- different encodings the same.
--
-- Why don't we parse `RespString` into `Data.Text.Text`? Well, the caller might
-- not actually need to decode it into text, and so we let the caller
-- decide. This way, we don't have to deal with encoding errors.
--
-- Similarly, we don't parse a `RespMap` into a `Data.HashMap.HashMap`,
-- because that would involve imposing our choice of data structure on
-- the caller. The caller might want to use `Data.HashMap.HashMap`,
-- `Data.Map.Map`, iterate over the elements, or just use the `lookup`
-- function.
data RespExpr
  = RespString !ByteString
  | RespBlob !ByteString
  | RespStreamingBlob !BSL.ByteString
  | RespStringError !ByteString
  | RespBlobError !ByteString
  | RespArray ![RespExpr]
  | RespInteger !Int64
  | RespNull
  | RespBool !Bool
  | RespDouble !Double
  | RespVerbatimString !ByteString
  | RespVerbatimMarkdown !ByteString
  | RespBigInteger !Integer
  | RespMap ![(RespExpr, RespExpr)]
  | RespSet ![RespExpr]
  | RespAttribute ![(RespExpr, RespExpr)] RespExpr
  deriving (Show, Eq, Ord, Generic)

data MessageSize
  = MSVariable
  | MSFixed Int

data NullableMessageSize
  = NMSVariable
  | NMSMinusOne
  | NMSFixed Int

-- | Parse a RESP3 message
parseMessage :: Scanner RespMessage
parseMessage = do
  c <- Scanner.anyChar8
  case c of
    '>' -> parsePush
    _ -> RespReply <$> parseExpression' c

-- | Parse a RESP3 expression
parseExpression :: Scanner RespExpr
parseExpression = Scanner.anyChar8 >>= parseExpression'

-- | Parse a RESP3 expression, taking its first char as a parameter
parseExpression' :: Char -> Scanner RespExpr
parseExpression' c = case c of
  '$' -> parseBlob
  '+' -> parseString
  '-' -> parseStringError
  ':' -> RespInteger <$> parseInteger
  '*' -> parseArray RespArray
  '_' -> RespNull <$ parseEol 
  '#' -> RespBool . (== 't') <$> Scanner.anyChar8 <* parseEol
  ',' -> parseDouble
  '!' -> parseBlobError
  '=' -> parseVerbatimString
  '(' -> RespBigInteger <$> parseInteger
  '%' -> RespMap <$> parseMap
  '~' -> parseArray RespSet
  '|' -> RespAttribute <$> parseMap <*> parseExpression
  _ -> fail $ "Unknown expression prefix: " <> show c

parsePush :: Scanner RespMessage
parsePush = do
  len <- parseMessageSize
  RespPush <$> parsePushType <*> replicateM (pred len) parseExpression

parsePushType :: Scanner ByteString
parsePushType = do
  c <- Scanner.anyChar8
  -- No idea whether this can be a simple string or not,
  -- the spec isn't specific enough.
  --
  -- The spec doesn't say that the push type *can't* be a
  -- streamed blob string (or null), but let's face it, only a sadist would
  -- return one of those. I'll try to get these possibilities excluded from
  -- the spec, but in the meantime, we're going to have to parse all the
  -- blobstrings.
  case c of
    '$' -> parseBlob' id lazyBsToStrict $ fail "Push message type can't be null"
    '+' -> parseLine
    _ -> fail "Invalid push message type"

parseMap :: Scanner [(RespExpr, RespExpr)]
parseMap = do
  len <- parseComplexMessageSize
  case len of
    MSFixed n -> replicateM n parseTwoEls
    MSVariable -> parseVarMapPairs

-- See https://github.com/redis/redis-specifications/blob/master/protocol/RESP3.md#streamed-aggregated-data-types
parseVarMapPairs :: Scanner [(RespExpr, RespExpr)]
parseVarMapPairs = do
  c <- Scanner.anyChar8
  case c of
    '.' -> [] <$ parseEol
    _ -> (:) <$> ((,) <$> parseExpression' c <*> parseExpression) <*> parseVarMapPairs

parseTwoEls :: Scanner (RespExpr, RespExpr)
parseTwoEls = (,) <$> parseExpression <*> parseExpression

-- See: https://github.com/redis/redis-specifications/issues/25
--    , https://github.com/redis/redis-specifications/issues/23
parseVerbatimString :: Scanner RespExpr
parseVerbatimString = do
  len <- parseMessageSize
  entireBlob <- Scanner.take len
  let body = BS8.drop 4 entireBlob
  parseEol
  case BS8.take 3 entireBlob of
    "txt" -> pure $ RespVerbatimString body
    "mkd" -> pure $ RespVerbatimMarkdown body
    _ -> fail "Unknown verbatim string type"

-- I suspect that this can't be streamed, or null
-- See: https://github.com/redis/redis-specifications/issues/23
parseBlobError :: Scanner RespExpr
parseBlobError = do
  len <- parseMessageSize
  RespBlobError <$> Scanner.take len <* parseEol

bsContains :: Char -> ByteString -> Bool
bsContains c = BS8.any (== c)

-- Scanning to NaN is a function so that we don't
-- feel guilty about inlining the patterns
parseLineAsNaN :: Scanner Double
parseLineAsNaN = (0 / 0) <$ parseLine

parseLineAsInf :: Scanner Double
parseLineAsInf = (1 / 0) <$ parseLine

-- (inf|-inf|nan|(+|-)?\d+(\.\d+)?([eE](+|-)?\d+))
--
-- Due to Redis bugs prior to 7.2, we also have to deal with
-- /(-)?nan(\(.*\))?/i, even though they're not part of the
-- RESP spec...
parseDouble :: Scanner RespExpr
parseDouble = do
  c <- Scanner.anyChar8
  RespDouble <$> case c of
    '+' -> go1 =<< Scanner.anyChar8
    '-' -> fmap negate $ go1 =<< Scanner.anyChar8
    'i' -> do
      -- Note: We're not validating that the rest of the line
      -- is actually "nf", because `,i` uniquely determines the
      -- set of valid responses.
      parseLineAsInf
    'n' -> parseLineAsNaN
    'N' -> parseLineAsNaN
    _ -> go1 c

  where
    -- takes first non-sign char of the significand
    go1 :: Char -> Scanner Double
    go1 'i' = parseLineAsInf
    go1 'n' = parseLineAsNaN
    go1 'N' = parseLineAsNaN
    go1 c1 = fromRational <$> do
      decStr <- Scanner.takeWhileChar8 $ not . (`bsContains` ".\reE")
      let dec = parseNatural1 c1 decStr :: Integer
      c2 <- Scanner.anyChar8
      case c2 of
        '\r' -> fromIntegral dec <$ expectChar '\n' 
        '.' -> do
          decStr1 <- Scanner.takeWhileChar8 $ not . (`bsContains` "\reE")
          let dec1 = fromIntegral (parseNatural' dec decStr1) / (10 ^ BS.length decStr1) :: Rational
          c3 <- Scanner.anyChar8
          case c3 of
            '\r' -> dec1 <$ expectChar '\n'
            _ {- c3 `elem` "eE" -} -> go2 dec1
        _ {- c3 `elem` "eE" -} -> go2 $ fromIntegral dec

    -- from first char of exponent (after [eE])
    go2 :: Rational -> Scanner Rational
    go2 n = do
      c <- Scanner.anyChar8
      (negExp, exponent') <- case c of
        '-' -> (True,) . parseNatural <$> parseLine
        '+' -> (False,) . parseNatural <$> parseLine
        _ {- isDigit c -} -> (False,) . parseNatural1 c <$> parseLine
      let expMul = fromIntegral (10 ^ (exponent' :: Integer) :: Integer) :: Rational
      pure $ if negExp then n / expMul else n * expMul

parseNatural :: Integral a => ByteString -> a
parseNatural = parseNatural' 0

parseNatural' :: Integral a => a -> ByteString -> a
parseNatural' = BS8.foldl' (\a b -> a * 10 + fromIntegral (digitToInt b))

parseNatural1 :: Integral a => Char -> ByteString -> a
parseNatural1 = parseNatural' . fromIntegral . digitToInt

-- RESP2 calls these 'multi bulk'
-- RESP3 calls it an 'array'
--
-- This is used to parse arrays and sets, meaning that we parse
-- "~-1\r\n" as RespNull, although this isn't a valid form in the spec.
parseArray :: ([RespExpr] -> RespExpr) -> Scanner RespExpr
parseArray construct = do
  messageSize <- parseComplexNullableMessageSize
  case messageSize of
    NMSFixed n -> construct <$> replicateM n parseExpression
    NMSMinusOne -> pure RespNull
    NMSVariable -> construct <$> parseVarArrayItems

-- See https://github.com/redis/redis-specifications/blob/master/protocol/RESP3.md#streamed-aggregated-data-types
parseVarArrayItems :: Scanner [RespExpr]
parseVarArrayItems = do
  c <- Scanner.anyChar8
  case c of
    '.' -> [] <$ parseEol
    _ -> (:) <$> parseExpression' c <*> parseVarArrayItems

-- RESP2 calls these 'bulk strings'
-- RESP3 calls them 'blob strings' (in the markdown, on the website they're still 'bulk strings')
parseBlob :: Scanner RespExpr
parseBlob = parseBlob' RespBlob RespStreamingBlob $ pure RespNull

-- general case for something that's pretty blobstring-like
parseBlob'
  :: (ByteString -> a)
  -> (BSL.ByteString -> a)
  -> Scanner a
  -> Scanner a
parseBlob' strictConstr lazyConstr nullConstr = do
  ms <- parseComplexNullableMessageSize
  case ms of
    NMSFixed n -> strictConstr <$> Scanner.take n <* parseEol
    NMSVariable -> lazyConstr . BSL.fromChunks <$> streamingBlobParts
    NMSMinusOne -> nullConstr

parseMessageSize :: Scanner Int
parseMessageSize = parseNatural <$> parseLine

-- Used for blobs and arrays
parseComplexNullableMessageSize :: Scanner NullableMessageSize
parseComplexNullableMessageSize = do
  line <- parseLine
  case line of
    "?" -> pure NMSVariable
    "-1" -> pure NMSMinusOne
    _ -> pure $ NMSFixed $ parseNatural line

-- Used for maps, attributes, sets
parseComplexMessageSize :: Scanner MessageSize
parseComplexMessageSize = do
  line <- parseLine
  case line of
    "?" -> pure MSVariable
    _ -> pure $ MSFixed $ parseNatural line

streamingBlobParts :: Scanner [ByteString]
streamingBlobParts = do
  expectChar ';'
  ms <- parseMessageSize
  case ms of
    0 -> pure mempty
    n -> (:) <$> Scanner.take n <* parseEol <*> streamingBlobParts

parseString :: Scanner RespExpr
parseString = RespString <$> parseLine

-- Cautious interpretation, until we can clarify that the
-- error tag is mandatory.
-- https://github.com/redis/redis-specifications/issues/24
parseStringError :: Scanner RespExpr
parseStringError = RespStringError <$> parseLine

parseInteger :: Integral a => Scanner a
parseInteger = do
  c <- Scanner.anyChar8
  case c of
    '+' -> parseNatural <$> parseLine
    '-' -> negate . parseNatural <$> parseLine
    _ -> parseNatural1 c <$> parseLine

parseLine :: Scanner ByteString
parseLine = Scanner.takeWhileChar8 (/= '\r') <* parseEol

expectChar :: Char -> Scanner ()
expectChar c = do
  d <- Scanner.anyChar8
  when (c /= d) $ fail $ "Expected " <> show c <> ", but got " <> show d

parseEol :: Scanner ()
parseEol = do
  expectChar '\r'
  expectChar '\n'
