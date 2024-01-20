{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

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

  -- The redis convention of including an error code prefix
  -- in the form "-PREFIX error message\r\n"
  -- is just that, a convention, and isn't part of the
  -- RESP protocol.
  | RespStringError Text

  -- TODO
  | RespBlobError ByteString

  | RespArray [RespReply]
  | RespInteger Int64
  | RespNull
  | RespBool Bool
  | RespDouble Double
  deriving (Show, Eq)

data MessageSize
  = MSVariable
  | MSMinusOne
  | MSFixed Int

reply :: Scanner RespReply
reply = Scanner.anyChar8 >>= scanTopLevelReply

scanTopLevelReply :: Char -> Scanner RespReply
scanTopLevelReply c = case c of
  '$' -> scanBlob
  '+' -> scanString
  '-' -> scanStringError
  ':' -> scanInteger
  '*' -> scanArray
  '_' -> scanEol $> RespNull
  '#' -> RespBool . (== 't') <$> Scanner.anyChar8 <* scanEol
  ',' -> scanDouble
  _ -> fail "Unknown reply type"

bsContains :: Char -> ByteString -> Bool
bsContains c = BS8.any (== c)

scanDouble :: Scanner RespReply
scanDouble = do
  c <- Scanner.anyChar8
  RespDouble . fromRational <$> case c of
    '+' -> go1 =<< Scanner.anyChar8
    '-' -> fmap negate $ go1 =<< Scanner.anyChar8
    _ -> go1 c

  where
    go1 :: Char -> Scanner Rational
    go1 c1 = do
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
            '\r' -> pure dec1
            _ {- c3 `elem` "eE" -} -> go2 dec1
        _ {- c3 `elem` "eE" -} -> go2 $ fromIntegral dec

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
  messageSize <- scanComplexMessageSize
  case messageSize of
    MSFixed n -> RespArray <$> replicateM n reply
    MSMinusOne -> pure RespNull
    MSVariable -> RespArray <$> scanVarArrayItems

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
  ms <- scanComplexMessageSize
  case ms of
    MSFixed n -> RespBlob <$> Scanner.take n <* scanEol
    MSVariable -> RespStreamingBlob . BSL.fromChunks <$> streamingBlobParts
    MSMinusOne -> pure RespNull

scanMessageSize :: Scanner Int
scanMessageSize = parseNatural <$> scanLine

-- Used for blobs and arrays
scanComplexMessageSize :: Scanner MessageSize
scanComplexMessageSize = do
  line <- scanLine
  case line of
    "?" -> pure MSVariable
    "-1" -> pure MSMinusOne
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

scanStringError :: Scanner RespReply
scanStringError = RespStringError . Text.decodeUtf8 <$> scanLine

scanInteger :: Scanner RespReply
scanInteger = RespInteger . parseNatural <$> scanLine

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
