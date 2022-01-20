{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.PasreAssignmentFileOC where

import GHC.Generics (Generic)

import Data.Map (Map)
import qualified Data.ByteString as BS
import qualified Data.Bits as B

import Data.Map (Map)
import qualified Data.Map as M

-- 文字列のパーサ
-- parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Combinator as P
import qualified Text.Parsec.Char as P
import qualified Text.Parsec as P




decodeOC2ATS1 :: ByteString -> Array SheetNameOC2ATS1 ByteString


pasreAssignmentFileOC2ATS1 :: Parser [(String, SheetNameOC2ATS1, Int)] -> 
-- 引数は？
-- pasreAssignmentFileOC2ATS1

toBitStatus :: [(String, SheetNameOC2ATS1, Int)] -> Array SheetNameOC2ATS1 ByteString -> Map SheetNameOC2ATS1 [(String, Bool)]
-- toBitStatus pasreAssignmentFile decodeOC2ATS1 =  

-- (!) :: Int -> ByteString -> Word8
-- testBit :: Bits a => a -> Int -> Bool

data SheetNameOC2ATS1
    = Li1
    | Li2
    | Li3
    | Li4
    | Si1
    | Si2
    | Si3
    | Si4
    | Si5
    | H
    | P
    deriving (Show, Eq, Ix)


data SheetNameOC2ATS2
    = Lo
    | So
    | SRAM
    | M
    deriving (Show, Eq, Ix)

data SheetNameOC2ATS3
    = I6
    | O1
    | O2
    | O3
    | O4
    | O5
    | O6
    | A
    deriving (Show, Eq, Ix)


