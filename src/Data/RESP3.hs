{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.RESP3
  ( RespReply(..)
  , reply
  ) where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as BSL
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text.Encoding    as Text
import qualified Scanner               as Scanner

import Data.ByteString (ByteString)
import Data.Char       (digitToInt)
import Data.Int        (Int64)
import Data.Text       (Text)
import Scanner         (Scanner)

data RespReply
  = RespString Text
  | RespBlob ByteString
  | RespStreamingBlob BSL.ByteString
  -- TODO will this always be text?
  | RespStringError Text
  | RespBinaryError ByteString
  | RespInteger Int64
  deriving (Show, Eq)

data MessageSize
  = MSVariable
  | MSFixed Int

reply :: Scanner RespReply
reply = do
  c <- Scanner.anyChar8
  case c of
    '$' -> scanBlob
    '+' -> scanString
    '-' -> scanError
    ':' -> scanInteger
    _ -> fail "Unknown reply type"

scanMessageSize :: Scanner MessageSize
scanMessageSize = do
  line <- scanLine
  case line of
    "?" -> pure MSVariable
    _ -> pure $ MSFixed $ parseNatural line

parseNatural :: Integral a => ByteString -> a
parseNatural = BS8.foldl' (\a b -> a * 10 + fromIntegral (digitToInt b)) 0

scanBlob :: Scanner RespReply
scanBlob = RespBlob <$> do
  ms <- scanMessageSize
  case ms of
    MSFixed n -> Scanner.take n <* scanEol
    MSVariable -> streamingBlobParts

streamingBlobParts :: Scanner ByteString
streamingBlobParts = do
  Scanner.char8 ';'
  undefined

-- TODO check RESP3 spec
scanString :: Scanner RespReply
scanString = RespString . Text.decodeUtf8 <$> scanLine

-- TODO check RESP3 spec
scanError :: Scanner RespReply
scanError = RespStringError . Text.decodeUtf8 <$> scanLine

-- TODO check RESP3 spec
scanInteger :: Scanner RespReply
scanInteger = RespInteger . parseNatural <$> scanLine

scanLine :: Scanner ByteString
scanLine = Scanner.takeWhileChar8 (/= '\r') <* scanEol

scanEol :: Scanner ()
scanEol = do
  Scanner.char8 '\r'
  Scanner.char8 '\n'
