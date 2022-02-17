{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.PasreAssignmentFileOC where

import GHC.Generics (Generic)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Bits as B

import Data.Array
import Control.Applicative

import Data.List
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

getOC2ATS2 :: Get (Map SheetName ByteString)
getOC2ATS2 = do
    Serial.skip 76
    lo <- Serial.getBytes 196
    so <- Serial.getBytes 106
    sram <- Serial.getBytes 54
    mem <- Serial.getBytes 512
    return $ M.fromList
        [ (Lo, lo)
        , (So, so)
        , (SRAM, sram)
        , (Mem, mem)
        ]

getOC2ATS3 :: Get (Map SheetName ByteString)
getOC2ATS3 = do
    Serial.skip 76
    i1 <- Serial.getBytes 24
    i2 <- Serial.getBytes 24
    i3 <- Serial.getBytes 24
    i4 <- Serial.getBytes 24
    i5 <- Serial.getBytes 24
    i6 <- Serial.getBytes 24
    o1 <- Serial.getBytes 24
    o2 <- Serial.getBytes 24
    o3 <- Serial.getBytes 24
    o4 <- Serial.getBytes 24
    o5 <- Serial.getBytes 24
    o6 <- Serial.getBytes 24
    alt <- Serial.getBytes 896
    return $ M.fromList
        [ (I1, i1)
        , (I2, i2)
        , (I3, i3)
        , (I4, i4)
        , (I5, i5)
        , (I6, i6)
        , (O1, o1)
        , (O2, o2)
        , (O3, o3)
        , (O4, o4)
        , (O5, o5)
        , (O6, o6)
        , (Alt, alt)
        ]
   
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
    return (str, sheet, pos - 1)

toBitStatus :: [(String, SheetName, Int)] -> Map SheetName ByteString -> Map SheetName [(String, Bool)]
toBitStatus assignments sheets = M.intersectionWith testBits sheets assignmentPerSheet
 where assignmentPerSheet :: Map SheetName [(String, Int)]
       assignmentPerSheet = allotMap $ map toKeyValuePair assignments
        where toKeyValuePair :: (String, SheetName, Int) -> (SheetName, (String, Int)) 
              toKeyValuePair (str, sheet, n) = (sheet,(str, n))

allotMap :: Ord k => [(k, val)] -> Map k [val]
allotMap as = foldl' f mempty as
 where f mp (k, val) = M.insertWith (++) k [val] mp
 
testBits :: ByteString -> [(String, Int)] -> [(String, Bool)]
testBits bstr assignments = map testOneBit assignments
 where testOneBit :: (String, Int) -> (String, Bool)
       testOneBit (str, n) = (str, b)
        where b = B.testBit (BS.index bstr q) r
              (q, r) = n `quotRem` 8


-- M.intersectionWith :: Ord k => (a -> b -> c) -> Map k a -> Map k b -> Map k c
-- M.insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a

-- testBit :: Word8 -> Int -> Bool
-- index :: HasCallStack => ByteString -> Int -> Word8

-- singleton


-- quotRem :: Integral => a -> (a, a)
-- ↑あまりの定義が負の場合 remとmodで異なる

-- (!) :: Int -> ByteString -> Word8



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




