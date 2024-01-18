{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.RESP3
  ( reply
  ) where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text.Encoding    as Text
import qualified Scanner               as Scanner

import Control.Monad
import Data.ByteString (ByteString)
import Data.Char       (digitToInt)
import Data.Int        (Int64)
import Data.Text       (Text)
import Scanner         (Scanner)

data Reply
  = String Text
  | Blob ByteString
  -- TODO will this always be text?
  | StringError Text
  | BinaryError ByteString
  | Integer Int64
  | Bulk (Maybe ByteString)
  | Multi (Maybe [Reply])
  deriving (Show, Eq)

data MessageSize
  = MSVariable
  | MSFixed Int

reply :: Scanner Reply
reply = do
  c <- Scanner.anyChar8
  case c of
    '$' -> scanBlobstring
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

scanBlobstring :: Scanner Reply
scanBlobstring = Blob <$> do
  ms <- scanMessageSize
  case ms of
    MSFixed n -> Scanner.take n <* scanEol
    MSVariable -> streamingBlobstringParts

streamingBlobstringParts :: Scanner ByteString
streamingBlobstringParts = do
  Scanner.char8 ';'
  undefined

-- TODO check RESP3 spec
scanString :: Scanner Reply
scanString = String . Text.decodeUtf8 <$> scanLine

-- TODO check RESP3 spec
scanError :: Scanner Reply
scanError = StringError . Text.decodeUtf8 <$> scanLine

-- TODO check RESP3 spec
scanInteger :: Scanner Reply
scanInteger = Integer . parseNatural <$> scanLine

scanLine :: Scanner ByteString
scanLine = Scanner.takeWhileChar8 (/= '\r') <* scanEol

scanEol :: Scanner ()
scanEol = do
  Scanner.char8 '\r'
  Scanner.char8 '\n'
