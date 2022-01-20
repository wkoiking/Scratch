{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.PasreAssignmentFileOC where

import GHC.Generics (Generic)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Bits as B

import Data.Array
import Control.Applicative

import Data.Map (Map)
import qualified Data.Map as M

-- バイナリデータ(decode）
-- cereal -- serializeからきてる
import Data.Serialize (Get, Putter)
import qualified Data.Serialize as Serial

-- 文字列のパーサ
-- parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Combinator as P
import qualified Text.Parsec.Char as P
import qualified Text.Parsec as P

import qualified Text.ParserCombinators.Parsec.Numeric as P (int)

getOC2ATS1 :: Get (Map SheetName ByteString)
getOC2ATS1 = do
    Serial.skip 76
    li1 <- Serial.getBytes 196
    li2 <- Serial.getBytes 196
    li3 <- Serial.getBytes 196
    li4 <- Serial.getBytes 196
    si1 <- Serial.getBytes 56
    si2 <- Serial.getBytes 56
    si3 <- Serial.getBytes 56
    si4 <- Serial.getBytes 56
    si5 <- Serial.getBytes 56
    hold <- Serial.getBytes 54
    pulse <- Serial.getBytes 210
    return $ M.fromList
        [ (Li1, li1)
        , (Li2, li2)
        , (Li3, li3)
        , (Li4, li4)
        , (Si1, si1)
        , (Si2, si2)
        , (Si3, si3)
        , (Si4, si4)
        , (Si5, si5)
        , (Hold, hold)
        , (Pulse, pulse)
        ]


decodeOC2ATS1 :: ByteString -> Map SheetName ByteString
decodeOC2ATS1 bstr = undefined

parseSheetName :: Parser SheetName
parseSheetName = do
    n <- P.int
    case n of
        1 -> return I1
        2 -> return I2
        3 -> return I3
        4 -> return I4
        5 -> return I5
        6 -> return I6
        7 -> return Li1
        8 -> return Li2
        9 -> return Li3
        10 -> return Li4
        11 -> return Si1
        12 -> return Si2
        13 -> return Si3
        14 -> return Si4
        15 -> return Si5
        16 -> return Hold
        17 -> return Pulse
        18 -> return O1
        19 -> return O2
        20 -> return O3
        21 -> return O4
        22 -> return O5
        23 -> return O6
        24 -> return Lo
        25 -> return So
        26 -> return SRAM
        27 -> return Mem
        28 -> return Alt
        _ -> fail "undefined sheet number for SheetName"


parseAssignmentFile :: Parser [(String, SheetName, Int)]
parseAssignmentFile = parseLine `P.endBy` P.newline 

parseLine :: Parser (String, SheetName, Int)
parseLine = do
    -- many :: Parser a -> Parser [a]
    str <- many (P.noneOf ",")
    P.char ','
    sheet <-  parseSheetName
    P.char ','
    pos <- P.int
    return (str, sheet, pos)

toBitStatusOC3ATS1 :: [(String, SheetName, Int)] -> Map SheetName ByteString -> Map SheetName [(String, Bool)]
toBitStatusOC3ATS1 = undefined

-- (!) :: Int -> ByteString -> Word8
-- testBit :: Bits a => a -> Int -> Bool

data SheetName
    = I1
    | I2
    | I3
    | I4
    | I5
    | I6
    | Li1
    | Li2
    | Li3
    | Li4
    | Si1
    | Si2
    | Si3
    | Si4
    | Si5
    | Hold
    | Pulse
    | O1
    | O2
    | O3
    | O4
    | O5
    | O6
    | Lo
    | So
    | SRAM
    | Mem
    | Alt
    deriving (Show, Eq, Ord, Enum, Bounded, Ix)


