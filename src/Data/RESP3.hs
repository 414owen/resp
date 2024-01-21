{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
-- We don't declare any spines
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TupleSections     #-}

module Data.RESP3
  ( RespReply(..)
  , reply
  ) where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text.Encoding    as Text
import qualified Scanner               as Scanner

import Data.ByteString      (ByteString)
import Data.Char            (digitToInt)
import Data.Int             (Int64)
import Data.Text            (Text)
import Scanner              (Scanner)
import Control.Monad        (when, replicateM)
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Functor (($>))

data RespReply
  = RespString Text
  | RespBlob ByteString
  | RespStreamingBlob LazyByteString
  | RespStringError Text
  | RespBlobError ByteString
  | RespArray [RespReply]
  | RespInteger Int64
  | RespNull
  | RespBool Bool
  | RespDouble Double
  | RespVerbatimString Text
  | RespVerbatimMarkdown Text
  | RespBigInteger Integer
  | RespMap [(RespReply, RespReply)]
  deriving (Show, Eq)

data MessageSize
  = MSVariable
  | MSFixed Int

data NullableMessageSize
  = NMSVariable
  | NMSMinusOne
  | NMSFixed Int

reply :: Scanner RespReply
reply = Scanner.anyChar8 >>= scanTopLevelReply

-- Top level meaning it parses all the forms that start with one
-- of the top level introducer chars. Feel free to use it elsewhere.
scanTopLevelReply :: Char -> Scanner RespReply
scanTopLevelReply c = case c of
  '$' -> scanBlob
  '+' -> scanString
  '-' -> scanStringError
  ':' -> RespInteger <$> scanInteger
  '*' -> scanArray
  '_' -> scanEol $> RespNull
  '#' -> RespBool . (== 't') <$> Scanner.anyChar8 <* scanEol
  ',' -> scanDouble
  '!' -> scanBlobError
  '=' -> scanVerbatimString
  '(' -> RespBigInteger <$> scanInteger
  '%' -> RespMap <$> scanMap
  _ -> fail "Unknown reply type"

scanMap :: Scanner [(RespReply, RespReply)]
scanMap = do
  len <- scanComplexMessageSize
  case len of
    MSFixed n -> replicateM n scanTwoEls
    MSVariable -> scanVarMapPairs

-- See https://github.com/redis/redis-specifications/blob/master/protocol/RESP3.md#streamed-aggregated-data-types
scanVarMapPairs :: Scanner [(RespReply, RespReply)]
scanVarMapPairs = do
  c <- Scanner.anyChar8
  case c of
    '.' -> scanEol $> []
    _ -> (:) <$> ((,) <$> scanTopLevelReply c <*> reply) <*> scanVarMapPairs

scanTwoEls :: Scanner (RespReply, RespReply)
scanTwoEls = (,) <$> reply <*> reply

-- See: https://github.com/redis/redis-specifications/issues/25
--    , https://github.com/redis/redis-specifications/issues/23
scanVerbatimString :: Scanner RespReply
scanVerbatimString = do
  len <- scanMessageSize
  entireBlob <- Scanner.take len
  let body = Text.decodeUtf8 $ BS8.drop 4 entireBlob
  case BS8.take 3 entireBlob of
    "txt" -> pure $ RespVerbatimString body
    "mkd" -> pure $ RespVerbatimMarkdown body
    _ -> fail "Unknown verbatim string type"

-- I suspect that this can't be streamed, or null
-- See: https://github.com/redis/redis-specifications/issues/23
scanBlobError :: Scanner RespReply
scanBlobError = do
  len <- scanMessageSize
  RespBlobError <$> Scanner.take len <* scanEol

bsContains :: Char -> ByteString -> Bool
bsContains c = BS8.any (== c)

-- Scanning to NaN is a function so that we don't
-- feel guilty about inlining the patterns
scanLineAsNaN :: Scanner Double
scanLineAsNaN = scanLine $> (0 / 0)

scanLineAsInf :: Scanner Double
scanLineAsInf = scanLine $> (1 / 0)

-- (inf|-inf|nan|(+|-)?\d+(\.\d+)?([eE](+|-)?\d+))
--
-- Due to Redis bugs prior to 7.2, we also have to deal with
-- /(-)?nan(\(.*\))?/i, even though they're not part of the
-- RESP spec...
scanDouble :: Scanner RespReply
scanDouble = do
  c <- Scanner.anyChar8
  RespDouble <$> case c of
    '+' -> go1 =<< Scanner.anyChar8
    '-' -> fmap negate $ go1 =<< Scanner.anyChar8
    'i' -> do
      -- Note: We're not validating that the rest of the line
      -- is actually "nf", because `,i` uniquely determines the
      -- set of valid responses.
      scanLineAsInf
    'n' -> scanLineAsNaN
    'N' -> scanLineAsNaN
    _ -> go1 c

  where
    -- takes first non-sign char of the significand
    go1 :: Char -> Scanner Double
    go1 'i' = scanLineAsInf
    go1 'n' = scanLineAsNaN
    go1 'N' = scanLineAsNaN
    go1 c1 = fromRational <$> do
      decStr <- Scanner.takeWhileChar8 $ not . (`bsContains` ".\reE")
      let dec = parseNatural1 c1 decStr :: Integer
      c2 <- Scanner.anyChar8
      case c2 of
        '\r' -> expectChar '\n' $> fromIntegral dec
        '.' -> do
          decStr1 <- Scanner.takeWhileChar8 $ not . (`bsContains` "\reE")
          let dec1 = fromIntegral (parseNatural' dec decStr1) / (10 ^ BS.length decStr1) :: Rational
          c3 <- Scanner.anyChar8
          case c3 of
            '\r' -> expectChar '\n' $> dec1
            _ {- c3 `elem` "eE" -} -> go2 dec1
        _ {- c3 `elem` "eE" -} -> go2 $ fromIntegral dec

    -- from first char of exponent (after [eE])
    go2 :: Rational -> Scanner Rational
    go2 n = do
      c <- Scanner.anyChar8
      (negExp, exponent') <- case c of
        '-' -> (True,) . parseNatural <$> scanLine
        '+' -> (False,) . parseNatural <$> scanLine
        _ {- isDigit c -} -> (False,) . parseNatural1 c <$> scanLine
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
scanArray :: Scanner RespReply
scanArray = do
  messageSize <- scanComplexNullableMessageSize
  case messageSize of
    NMSFixed n -> RespArray <$> replicateM n reply
    NMSMinusOne -> pure RespNull
    NMSVariable -> RespArray <$> scanVarArrayItems

-- See https://github.com/redis/redis-specifications/blob/master/protocol/RESP3.md#streamed-aggregated-data-types
scanVarArrayItems :: Scanner [RespReply]
scanVarArrayItems = do
  c <- Scanner.anyChar8
  case c of
    '.' -> scanEol $> []
    _ -> (:) <$> scanTopLevelReply c <*> scanVarArrayItems

-- RESP2 calls these 'bulk strings'
-- RESP3 calls them 'blob strings' (in the markdown, on the website they're still 'bulk strings')
scanBlob :: Scanner RespReply
scanBlob = do
  ms <- scanComplexNullableMessageSize
  case ms of
    NMSFixed n -> RespBlob <$> Scanner.take n <* scanEol
    NMSVariable -> RespStreamingBlob . BSL.fromChunks <$> streamingBlobParts
    NMSMinusOne -> pure RespNull

scanMessageSize :: Scanner Int
scanMessageSize = parseNatural <$> scanLine

-- Used for blobs and arrays
scanComplexNullableMessageSize :: Scanner NullableMessageSize
scanComplexNullableMessageSize = do
  line <- scanLine
  case line of
    "?" -> pure NMSVariable
    "-1" -> pure NMSMinusOne
    _ -> pure $ NMSFixed $ parseNatural line

-- Used for maps, attributes, sets
scanComplexMessageSize :: Scanner MessageSize
scanComplexMessageSize = do
  line <- scanLine
  case line of
    "?" -> pure MSVariable
    _ -> pure $ MSFixed $ parseNatural line

streamingBlobParts :: Scanner [ByteString]
streamingBlobParts = do
  expectChar ';'
  ms <- scanMessageSize
  case ms of
    0 -> pure mempty
    n -> (:) <$> Scanner.take n <* scanEol <*> streamingBlobParts

scanString :: Scanner RespReply
scanString = RespString . Text.decodeUtf8 <$> scanLine

-- Cautious interpretation, until we can clarify that the
-- error tag is mandatory.
-- https://github.com/redis/redis-specifications/issues/24
scanStringError :: Scanner RespReply
scanStringError = RespStringError . Text.decodeUtf8 <$> scanLine

scanInteger :: Integral a => Scanner a
scanInteger = do
  c <- Scanner.anyChar8
  case c of
    '+' -> parseNatural <$> scanLine
    '-' -> negate . parseNatural <$> scanLine
    _ -> parseNatural1 c <$> scanLine

scanLine :: Scanner ByteString
scanLine = Scanner.takeWhileChar8 (/= '\r') <* scanEol

expectChar :: Char -> Scanner ()
expectChar c = do
  d <- Scanner.anyChar8
  when (c /= d) $ fail $ "Expected " <> show c <> ", but got " <> show d

scanEol :: Scanner ()
scanEol = do
  expectChar '\r'
  expectChar '\n'
