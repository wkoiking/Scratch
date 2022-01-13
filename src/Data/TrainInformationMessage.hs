{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.TrainInformationMessage where

import GHC.Generics (Generic)

-- 文字列のパーサ
-- parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Char as P
import qualified Text.Parsec as P

-- バイナリデータ(decode）
-- cereal -- serializeからきてる
import Data.Serialize (Get, Putter)
import qualified Data.Serialize as Serial
-- bit
import qualified Data.Bits.Coded as Bit
import qualified Data.Bits.Coding as Bit
-- バイナリデータ(encode）


-- qualified
import Text.Show.Pretty (ppShow)
import Data.List -- drop :: Int -> [a] -> [a]
import Data.Word
import Control.Monad
import Data.Map (Map)
import Data.Tuple (swap)
import qualified Data.Map as M

import Test.QuickCheck hiding (sample)
import Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

-- network
import Network.Socket (Socket, SockAddr)
import qualified Network.Socket as N 
import qualified Network.Socket.ByteString as N (recv, sendTo, send)

import qualified Data.ByteString as B (drop)

data TrainInformationMessage = TrainInformationMessage
    { commonPart :: CommonPart
    , trainPart :: [(RakeID, TrainInformation)]
    } deriving (Show, Eq)

skipBits :: Int -> BitGet ()
skipBits i = replicateM_ i Bit.getBit

getTrainInformationMessage :: Get TrainInformationMessage
getTrainInformationMessage = do
    commonPart <- getCommonPart
    trainPart <- replicateM 20 $ liftM2 (,) getRakeID getTrainInformation
-- getTrainInformation = liftM2 (,) getRakeID getTrainInformation
    return TrainInformationMessage{..}

-- getTrainInformation :: Get (RakeID, TrainInformation)
-- getTrainInformation = do
--     rakeID <- getRakeID
--     trainInfo <- getTrainInformation
--     return (rakeID, trainInfo)


getRakeID :: Get RakeID
getRakeID = liftM RakeID Serial.getWord16be
-- getRakeID = do
--     w16 <- getWord16be
--     return $ RakeID w16

data RakeID = RakeID Word16
    deriving (Show, Eq)

type BitGet = Bit.Coding Get

getTrainInformation :: Get TrainInformation
getTrainInformation = Bit.runDecode $ do
    statTrainReady <- Bit.getBit -- 出庫試験完了し、UTO走行可能かどうか
    skipBits 2
    statDeparture <- Bit.getBit -- 出発ボタンが押されたかどうか
    statSkipStop  <- Bit.getBit -- TASC制御無視ビット
    skipBits 3 -- notice of time
    statOCCRemoteCommandAck <- Bit.getAligned getOCCRemoteCommandAck -- Maybe OCCRemoteCommandAck
    -- ↑UTO以外では使わない。客車の電気、パンタグラフを上げたりetc.
    statTrainStopped <- Bit.getBit -- 車両が止まっているかどうか
    statTestTrack <- Bit.getBit -- 今、電車がTestTrackに居るかどうか
    skipBits 6
    Bit.getAligned $ Serial.skip 1
    
    statPassed <- Bit.getBit -- P0を通過したかどうか（通過後3パルスくらい送られてくる）※あまり使わない
    statOverspeed <- Bit.getBit -- 速度制限の超過
    statDoorEnabled <- Bit.getBit -- ドア開許可がないと開かない(安全性が求められる)
    skipBits 5
    skipBits 8 -- 後部

--   , _statTrainLocationBlockNoBuffer :: TrainLocation
    _ <- Bit.getAligned getTrainLocation
    statTrainLocationBlock <- Bit.getAligned getTrainLocation -- メートル単位の在線情報
    _ <- Bit.getAligned getTrainLocation
    _ <- Bit.getAligned getTrainLocation
    statTrainSpeed <- Bit.getAligned Serial.getWord8 -- 列車の速度(km/h)
    skipBits 8 -- 列車最高速度
    
    statDrivingMode <-  decodeDrivingMode :: BitGet (Maybe DrivingMode)
    statEBReleaseAck <- Bit.getBit -- OCCから非常ブレーキを緩解する(受理されると送ってくる)
    statEBReleaseCommand <- Bit.getBit -- オウム返しにするほうがこれ
    statDistanceToMA <- Bit.getBitsFrom 10 (0 :: Word16) -- 走行可能距離
    skipBits 2
    statRollingStockProfile <- decodeRollingStockProfile :: BitGet (Maybe RollingStockProfile) -- 車両種別(種別は案件による)
    statP0Stopped <- Bit.getBit -- P0上で止まってるとき
    statHeld <- Bit.getBit -- 出発抑止
    statRemoved <- Bit.getBit -- アンロケートに対するアック
    statSystem <- Bit.getBit -- どちらの系故障が送られてきているか
    statWakeupAck <- Bit.getBit -- スリープから起こすため
    statStanbyAck <- Bit.getBit -- スリープにするため
    statInitStatus <- decodeInitializationStatus -- 初期化に関するデータ
    statMasterSlave <- liftM2 (,) Bit.getBit Bit.getBit -- 正常であれば左右で真偽が違う
    statRunningDirection <- liftM2 (,) Bit.getBit Bit.getBit -- 列車の走ってる方向.正常であれば左右で真偽が違う
    statSleepMode <- decodeSleepModeStatus :: BitGet (Maybe SleepModeStatus)
    statResetResult <- liftM2 (,) Bit.getBit Bit.getBit
    statRegime <- decodePerformanceLevel :: BitGet (Maybe PerformanceLevel) -- 速さの調整ができる
    statDoorClosedStatus <- liftM2 (,) Bit.getBit Bit.getBit -- キロ程が上がってくる方向に対してのドア
    statIsRescueTrain <- Bit.getBit -- 現在、救援列車として走行しているかどうか(これにしないと救護車に近づけない)
    statDrivingStatus <- decodeDrivingStatus :: BitGet (Maybe DrivingStatus) -- ATOの時の制御状態
    statAdditionalInfo <- Bit.getAligned getAdditionalInformation -- OCCリモートコマンドに対して応答の負荷情報
    Bit.getAligned $ Serial.skip 4
    statEBReasonVOBC <- Bit.getAligned getEBReasonVOBC -- EBがかかった場合の原因(車上EB要因)
    statEBReasonSC <- Bit.getAligned getEBReasonSC -- EBをかけた場合の原因(地上EB要因)
    statOnBoardATCFailureInformation <- Bit.getAligned getOnBoardATCFailureInformation -- 故障情報
    
    return TrainInformation{..}

getOCCRemoteCommandAck :: Get OCCRemoteCommandAck
getOCCRemoteCommandAck = liftM OCCRemoteCommandAck Serial.getWord8
-- getOCCRemoteCommandAck = do
--     w8 <- getWord8
--     return $ OCCRemoteCommandAck w8

getTrainLocation :: Get TrainLocation
getTrainLocation = do
    frontBlockID <- Serial.getWord16be
    frontOffset  <- Serial.getWord16be
    rearBlockID  <- Serial.getWord16be
    rearOffset   <- Serial.getWord16be
    return TrainLocation{..}
    
decodeDrivingMode :: BitGet (Maybe DrivingMode)
decodeDrivingMode = do
    w8 <- Bit.getBitsFrom 2 ( 0b0000_0000 :: Word8)
    return $ case w8 of
        0b001 -> Just UTO 
        0b010 -> Just ATO 
        0b011 -> Just ATP 
        0b100 -> Just RM  
        _ -> Nothing
        
decodeRollingStockProfile :: BitGet (Maybe RollingStockProfile)
decodeRollingStockProfile = do
    w8 <- Bit.getBitsFrom 5 ( 0b0000_0000 :: Word8)
    return $ case w8 of
       1 -> Just FourCar
       2 -> Just SixCar
       3 -> Just EightCar
       _ -> Nothing


decodeInitializationStatus :: BitGet InitializationStatus
decodeInitializationStatus = do
    w8 <- Bit.getBitsFrom 1 ( 0b0000_0000 :: Word8)
    return $ case w8 of    
        0b00 -> NotInitialization
        0b01 -> InInitialization
        0b10 -> CompleteInitialization
        0b11 -> InitializationTiomeout

decodeSleepModeStatus :: BitGet (Maybe SleepModeStatus)
decodeSleepModeStatus = do
    w8 <- Bit.getBitsFrom 1 ( 0b0000_0000 :: Word8)
    return $ case w8 of
        0b00 -> Just NotSleep
        0b01 -> Just DuringTrainsition
        0b11 -> Just SleepMode
        _ -> Nothing

decodePerformanceLevel :: BitGet (Maybe PerformanceLevel)
decodePerformanceLevel = do
    w8 <- Bit.getBitsFrom 1 ( 0b0000_0000 :: Word8)
    return $ case w8 of    
        0b01 -> Just FastMode
        0b10 -> Just NormalMode
        0b11 -> Just SlowMode
        _ -> Nothing
        
decodeDrivingStatus :: BitGet (Maybe DrivingStatus)
decodeDrivingStatus = do
    w8 <- Bit.getBitsFrom 2 (0 :: Word8)
    return $ case w8 of
        0b000 -> Just Running
        0b001 -> Just Undershoot
        0b010 -> Just Overshoot
        0b011 -> Just TASC
        0b100 -> Just P0Stop
        0b101 -> Just NonP0Stop
        0b110 -> Just Inching
        _ -> Nothing

getAdditionalInformation :: Get AdditionalInformation
getAdditionalInformation = liftM AdditionalInformation Serial.getWord32be

getEBReasonVOBC :: Get EBReasonVOBC
getEBReasonVOBC = liftM EBReasonVOBC Serial.getWord16be

getEBReasonSC :: Get EBReasonSC
getEBReasonSC = liftM EBReasonSC Serial.getWord16be

getOnBoardATCFailureInformation:: Get OnBoardATCFailureInformation
getOnBoardATCFailureInformation = liftM OnBoardATCFailureInformation Serial.getWord32be


data TrainInformation = TrainInformation
    {
    -- statDoorFailToOpenClose :: Bool
--     , statWashModeStatus :: Bool
--     , statEBApplied :: Bool
      statTrainReady :: Bool
    , statDeparture :: Bool
    , statSkipStop  :: Bool
    , statOCCRemoteCommandAck :: OCCRemoteCommandAck
    , statTrainStopped :: Bool
    , statTestTrack :: Bool
    , statPassed :: Bool
    , statOverspeed :: Bool
    , statDoorEnabled :: Bool
--     , statTrainLocationBlockNoBuffer :: TrainLocation
    , statTrainLocationBlock :: TrainLocation
    , statTrainSpeed :: Word8
    , statDrivingMode :: (Maybe DrivingMode)
    , statEBReleaseAck :: Bool
    , statEBReleaseCommand :: Bool
    , statDistanceToMA :: Word16
    , statRollingStockProfile :: Maybe RollingStockProfile
    , statP0Stopped :: Bool
    , statHeld :: Bool
    , statRemoved :: Bool
    , statSystem :: Bool
    , statWakeupAck :: Bool
    , statStanbyAck :: Bool
    , statInitStatus :: InitializationStatus
    , statMasterSlave :: (Bool, Bool)
    , statRunningDirection :: (Bool, Bool)
    , statSleepMode :: (Maybe SleepModeStatus)
    , statResetResult :: (Bool, Bool)
    , statRegime :: (Maybe PerformanceLevel)
    , statDoorClosedStatus :: (Bool, Bool)
    , statIsRescueTrain :: Bool
    , statDrivingStatus :: (Maybe DrivingStatus)
    , statAdditionalInfo :: AdditionalInformation
    , statEBReasonVOBC :: EBReasonVOBC
    , statEBReasonSC :: EBReasonSC
    , statOnBoardATCFailureInformation :: OnBoardATCFailureInformation
--     , statOnBoardFailure :: OnBoardFailure
--     , statVrsIndentification :: VrsIdentification
--     , statVrsErrorCode :: VrsErrorCode
    } deriving (Show, Eq)

data DrivingMode
    = UTO -- unattended train operation
    | ATO -- automatic train orotection
    | ATP -- automatic train protection
    | RM  -- restricted manual
    deriving (Show, Eq)

data SleepModeStatus 
    = NotSleep
    | DuringTrainsition
    | SleepMode
    deriving (Show, Eq)
    
-- パンタグラフを落とした時、Sleepモードにして電力消費を抑える
-- 無線機などの冗長している箇所を落として、電力をあまり使わないようにする
-- スパークス（UTO）特有　人間が使ときはその限りじゃない

data PerformanceLevel
    = FastMode
    | NormalMode
    | SlowMode    
    deriving (Show, Eq)
-- 車上装置に対しての速度支持、案件によって種類は異なる（インドは5）
-- 遅延が起こると速くしたり、蛇行させることで電力消費を落としたりする

data DrivingStatus
    = Running
    | Undershoot
    | Overshoot
    | TASC
    | P0Stop
    | NonP0Stop
    | Inching
    deriving (Show, Eq)
-- P0Stop　…　定点停止（地上子と車上子）
-- 車上装置の今の制御状態

data OCCRemoteCommandAck = OCCRemoteCommandAck Word8
    deriving (Show, Eq)

data AdditionalInformation = AdditionalInformation Word32
    deriving (Show, Eq)

data TrainLocation =  TrainLocation
    { frontBlockID :: BlockID
    , frontOffset :: Word16
    , rearBlockID :: BlockID
    , rearOffset :: Word16
    } deriving (Show, Eq)

type BlockID = Word16

data EBReasonSC = EBReasonSC Word16
    deriving (Show, Eq)
    
data EBReasonVOBC = EBReasonVOBC Word16
    deriving (Show, Eq)

data OnBoardATCFailureInformation = OnBoardATCFailureInformation Word32
    deriving (Show, Eq)

data InitializationStatus
    = NotInitialization
    | InInitialization
    | CompleteInitialization
    | InitializationTiomeout
    deriving (Show, Eq)

data CommonPart = CommonPart
    { scStatus :: Bool　
    , axleCounterValid :: Bool
    } deriving (Show, Eq)

getCommonPart :: Get CommonPart
getCommonPart = Bit.runDecode $ do
   scStatus <- Bit.getBit
   axleCounterValid <- Bit.getBit
   skipBits $ 6 + 8 * 3
   return $ CommonPart{..}


getTimeOfDay :: Get TimeOfDay
getTimeOfDay = do  
    hour <- Serial.getWord8
    minute <- Serial.getWord8
    second <- Serial.getWord8
    -- レコードワイルドカードを使用するときは下のようにではなく、上でfmap fromIntegral getWord8 としてやる
    return $ TimeOfDay (fromIntegral hour) (fromIntegral minute) (fromIntegral second)

-- getTimeOfDay = 

parseInt :: Parser Int
parseInt = do
    digits <- P.many1 P.digit
    return $ read digits 


parseIntTuple :: Parser (Int, Int)
parseIntTuple = do
    n1 <- parseInt
    P.spaces -- 空白文字なら何でも読んでくれる(タブでもスペースでも)
    P.string "#"
    P.spaces
    n2 <- parseInt
    return (n1, n2)

parseTable :: Parser [(Int, Int)]
parseTable = parseIntTuple `P.endBy` P.endOfLine 

data MyTuple a b where
    MkTuple :: a -> b -> MyTuple a b
    deriving (Show, Eq, Generic)

data MyMaybe a where
    MkJust :: a -> MyMaybe a
    MkNothing :: MyMaybe a
    deriving (Show, Eq, Generic)



data MyEither a b where
    MkLeft :: a -> MyEither a b
    MkRight :: b -> MyEither a b
    deriving (Show, Eq, Generic)

data MyList a where
    (:|) :: a -> MyList a -> MyList a
    Nil :: MyList a
    deriving (Show, Eq, Generic)


parseDate :: Parser Date
parseDate = do
    dayStr <- P.count 2 P.digit
    P.string "/"
    monthStr <- P.count 2 P.digit
    P.string "/"
    yearStr <- P.count 4 P.digit
    return $ Date (read yearStr) (read monthStr) (read dayStr)

parseTime :: Parser Time
parseTime = do
    date <- parseDate
    P.skipMany1 P.space
    timeOfDay <- parseTimeOfDay
    return $ Time date timeOfDay

parseTimeOfDay :: Parser TimeOfDay
parseTimeOfDay = do
    hour <- parseInt
    P.string ":"
    minute <- parseInt
    P.string ":"
    second <- parseInt
    return $ TimeOfDay hour minute second

parseLogLine :: Parser LogLine
parseLogLine = do
    timeStamp <- parseTime
    P.spaces
    label <- P.many (P.noneOf ":")
    P.string ":"
    P.spaces
    message <- P.many (P.noneOf "\n")
    return $ LogLine timeStamp label message

parseLogFile :: Parser[LogLine]
parseLogFile = parseLogLine `P.endBy` P.endOfLine

getNextThreeDepartureMessage :: Get NextThreeDepartureMessage
getNextThreeDepartureMessage = do
    header <- getMessageHeader
    concoursePidInfos <- replicateM 3 getConcoursePidInfo
    return $ NextThreeDepartureMessage header concoursePidInfos

getMessageHeader :: Get MessageHeader
getMessageHeader = do
     messageSource <- getEquipmentID
     messageID <- getMessageID
     messageStationCode <- getStationCode
     return $ MessageHeader messageSource messageID messageStationCode
     -- {..}これで省略も可能

getMessageID :: Get MessageID
getMessageID = do
    n <- Serial.getWord8
    case n of
       2 -> return NextThreeDeparture
       3 -> return DeparturePlatfrom
       4 -> return ArrivalPlatfrom
       _ -> fail "undefined value" -- 時と場合によってエラーを返して失敗するか、Nothingで成功させるかは変える


getStationCode :: Get StationCode
getStationCode = do
    n <- Serial.getWord8
    case M.lookup n mapDecodeStationCode of
        Just stCode -> return stCode
        Nothing -> fail"undefined getStationCode"
    


mapDecodeStationCode :: Map Word8 StationCode
mapDecodeStationCode = M.fromList asocDecodeStationCode

mapEncodeStationCode :: Map StationCode Word8
mapEncodeStationCode = M.fromList $ map swap asocDecodeStationCode

asocDecodeStationCode :: [(Word8 , StationCode)]
asocDecodeStationCode =  zip [31 ..] [JPW ..]


getEquipmentID :: Get EquipmentID
getEquipmentID = do
    n <- Serial.getWord8
    case n of
       1 -> return ATS
       2 -> return PID
       _ -> fail "undefined EquipmentID"

getRollingStockProfile :: Get (Maybe RollingStockProfile)
getRollingStockProfile = do
    n <- Serial.getWord8
    return $ case n of
       1 -> Just FourCar
       2 -> Just SixCar
       3 -> Just EightCar
       _ -> Nothing

getPlatformNumber :: Get PlatformNumber
getPlatformNumber = do
    n <- Serial.getWord8
    return $ PlatformNumber n

getConcoursePidInfo :: Get ConcoursePidInfo
getConcoursePidInfo = do
    pidRollingStockProfile <- getRollingStockProfile
    pidPlatformNumber <- getPlatformNumber
    pidTimeOfDay <- getTimeOfDay
    pidDestinationStation <- getStationCode
    return $ ConcoursePidInfo pidRollingStockProfile pidPlatformNumber pidTimeOfDay pidDestinationStation


putTimeOfDay :: Putter TimeOfDay
putTimeOfDay (TimeOfDay hour minute second) = do
    Serial.putWord8 $ fromIntegral hour
    Serial.putWord8 $ fromIntegral minute
    Serial.putWord8 $ fromIntegral second

-- getTimeOfDay :: Get TimeOfDay
-- getTimeOfDay = do  
--     hour <- Serial.getWord8
--     minute <- Serial.getWord8
--     second <- Serial.getWord8
--     -- レコードワイルドカードを使用するときは下のようにではなく、上でfmap fromIntegral getWord8 としてやる
--     return $ TimeOfDay (fromIntegral hour) (fromIntegral minute) (fromIntegral second)


putNextThreeDepartureMessage :: Putter NextThreeDepartureMessage
putNextThreeDepartureMessage (NextThreeDepartureMessage header concoursePidInfos ) = do
    putMessageHeader header
    mapM_ putConcoursePidInfo concoursePidInfos

-- getNextThreeDepartureMessage :: Get NextThreeDepartureMessage
-- getNextThreeDepartureMessage = do
--     header <- getMessageHeader
--     concoursePidInfos <- replicateM 3 getConcoursePidInfo
--     return $ NextThreeDepartureMessage header concoursePidInfos


putMessageHeader :: Putter MessageHeader
putMessageHeader (MessageHeader messageSorce messageID messageStationCode) = do
    putEquipmentID messageSorce
    putMessageID messageID
    putStationCode messageStationCode

-- data MessageHeader = MessageHeader
--     { messageSource :: EquipmentID 
--     , messageID :: MessageID
--     , messageStationCode :: StationCode
--     } deriving (Show)



putMessageID :: Putter MessageID
putMessageID n = Serial.putWord8 $ case n of
    NextThreeDeparture -> 2
    DeparturePlatfrom  -> 3 
    ArrivalPlatfrom -> 4 
       
putStationCode :: Putter StationCode
putStationCode stCode =  case M.lookup stCode mapEncodeStationCode of
    Just code -> Serial.putWord8 code
    Nothing -> return ()

    


putEquipmentID :: Putter EquipmentID
putEquipmentID n = Serial.putWord8 $ case n of
    ATS -> 1
    PID -> 2

putRollingStockProfile :: Putter (Maybe  RollingStockProfile)
putRollingStockProfile mProfile = Serial.putWord8 $ case mProfile of
    Nothing       -> 0
    Just FourCar  -> 1
    Just SixCar   -> 2
    Just EightCar -> 3 

putPlatformNumber :: Putter PlatformNumber
putPlatformNumber (PlatformNumber n) = Serial.putWord8 n
-- Serial.putWord8 . unPlatformNumber 

putConcoursePidInfo :: Putter ConcoursePidInfo
putConcoursePidInfo (ConcoursePidInfo profile pl time dstStCode) = do
    putRollingStockProfile profile
    putPlatformNumber pl
    putTimeOfDay time
    putStationCode dstStCode

-- data ConcoursePidInfo = ConcoursePidInfo
--     { pidRollingStockProfile :: Maybe RollingStockProfile
--     , pidPlatformNumber :: PlatformNumber
--     , pidTimeOfDay :: TimeOfDay
--     , pidDestinationStation :: StationCode
--     } deriving (Show)

data NextThreeDepartureMessage = NextThreeDepartureMessage
    { header :: MessageHeader
    , concoursePidInfos :: [ConcoursePidInfo]
    } deriving (Show, Generic, Eq)-- Ord

-- ラウンドトリップテスト
prop_put_getNextThreeDepartureMessage :: NextThreeDepartureMessage -> Bool
prop_put_getNextThreeDepartureMessage a = Right a == eNextThreeDepartureMessage
 where bstr = Serial.runPut $ putNextThreeDepartureMessage a       
       eNextThreeDepartureMessage :: Either String NextThreeDepartureMessage
       eNextThreeDepartureMessage = Serial.runGet getNextThreeDepartureMessage bstr

prop_put_getConcoursePidInfo :: ConcoursePidInfo -> Bool
prop_put_getConcoursePidInfo a = Right a == eVal
 where bstr = Serial.runPut $ putConcoursePidInfo a       
       eVal = Serial.runGet getConcoursePidInfo bstr

-- data ConcoursePidInfo = ConcoursePidInfo
--     { pidRollingStockProfile :: Maybe RollingStockProfile
--     , pidPlatformNumber :: PlatformNumber
--     , pidTimeOfDay :: TimeOfDay
--     , pidDestinationStation :: StationCode
--     } deriving (Show, Eq, Generic)
--     
 
data MessageHeader = MessageHeader
    { messageSource :: EquipmentID 
    , messageID :: MessageID
    , messageStationCode :: StationCode
    } deriving (Show, Eq, Generic)

-- ラウンドトリップテスト
prop_put_getMessageHeader :: MessageHeader -> Bool
prop_put_getMessageHeader mh = Right mh == eMessageHeader
 where bstr = Serial.runPut $ putMessageHeader mh
       eMessageHeader :: Either String MessageHeader
       eMessageHeader = Serial.runGet getMessageHeader bstr
       

data MessageID
   = NextThreeDeparture
   | ArrivalPlatfrom
   | DeparturePlatfrom
   deriving (Show, Eq, Generic)

data StationCode
    = JPW
    | DBMR
    | DSHP
    | PALM
    | SABR
    | IGDA
    | SKVR
    | VTVR
    | MIRK
    | RKPM
    | IIT
    | HKS
    | PSPK
    | CDLI
    | GKEI
    | NUEE
    | KJMD
    | OKNS
    | IWNR
    | JANR
    | OVA
    | JLA
    | KIKJ
    | OKBS
    | BCGN
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)

data RollingStockProfile
    = FourCar
    | SixCar
    | EightCar
    deriving (Show, Eq, Generic)

data EquipmentID = ATS | PID deriving (Show, Eq, Generic)
    
data PlatformNumber = PlatformNumber
    { unPlatformNumber :: Word8
    } deriving (Show, Eq, Generic)

data ConcoursePidInfo = ConcoursePidInfo
    { pidRollingStockProfile :: Maybe RollingStockProfile
    , pidPlatformNumber :: PlatformNumber
    , pidTimeOfDay :: TimeOfDay
    , pidDestinationStation :: StationCode
    } deriving (Show, Eq, Generic)
    

data LogLine = LogLine
    { timeStamp :: Time
    , label :: String
    , message :: String
    } deriving (Show, Eq, Generic)

data Date = Date
    { year :: Int
    , month :: Int
    , day ::  Int
    } deriving (Show, Eq, Generic)

data TimeOfDay = TimeOfDay
    { hour :: Int
    , minute :: Int
    , second ::  Int
    } deriving (Show, Eq, Generic)

data Time = Time
    { date :: Date
    , timeOfDay :: TimeOfDay
    } deriving (Show, Eq, Generic)


is9oClock :: LogLine -> Bool
is9oClock logline  = hour (timeOfDay $ timeStamp $ logline) == 9
-- is9oClock = (==9) . hour . timeOfDay . timeStamp





instance Functor MyList where
    fmap = myMap

instance Applicative MyList where
    pure x = mySingleton x
    (<*>) = liftA2 id 
     where liftA2 op xs ys = myConcat $ myMap f xs
            where f x = myMap (x `op`) ys

instance Monad MyList where
    m >>= f = myConcat $ myMap f m
    m1 >> m2 = myConcat $ myMap (const m2) m1
    -- return = pure
someList :: MyList (MyTuple Int Int)
someList = do
    x <- myEnumFromTo 1 3
    y <- myEnumFromTo 4 5
    return $ MkTuple x y

data MyBool where
    MkTrue :: MyBool
    MkFalse :: MyBool
    deriving (Show, Eq, Generic)

-- instance Eq MyBool where
--     MkTrue == MkTrue = True
--     MkFalse == MkFalse = True
--     _ == _ = False

instance Functor MyMaybe where
    fmap f MkNothing = MkNothing
    fmap f (MkJust x) = MkJust $ f x


instance Applicative MyMaybe where
    -- pure :: a -> MkMaybe a
    pure = MkJust
    (<*>) = liftA2 id
--     liftA2 :: (a -> a -> a) -> MkMaybe a -> MkMaybe a -> MkMaybe a
     where
         liftA2 op MkNothing _ = MkNothing
         liftA2 op _ MkNothing = MkNothing
         liftA2 op (MkJust a) (MkJust b) = MkJust $ a `op` b

instance Monad MyMaybe where
    MkNothing >> _ = MkNothing
    MkJust x >> m = m
    MkNothing >>= _ =  MkNothing
    MkJust x >>= f = f x
    return = MkJust
    

infixr :|


someList1 :: MyList Int
someList1 = 1 :| 2 :| 3 :| Nil

someList2 :: MyList Int
someList2 = 7 :| 0 :| 3 :| 4 :|Nil

-- someList2 :: MyList Char
-- someList2 = 'a' :|  :| 3 :| 4 :|Nil 

myHead ::  MyList a -> a
-- myHead Nil = Nil
myHead (x :| xs) = x

(+++) :: MyList a -> MyList a -> MyList a
Nil +++ ys = ys
(x :| xs) +++ ys =  x :| (xs +++ ys)    


myReverse :: MyList a -> MyList a
myReverse Nil = Nil
myReverse (x :| xs) =  myReverse xs +++ mySingleton x

prop_myReverse :: MyList Int -> Bool
prop_myReverse xs = myReverse (myReverse xs) == xs

mySingleton :: a -> MyList a
mySingleton a = a :| Nil


myTail ::  MyList a -> MyList a
myTail (x :| xs) = xs

myLast ::  MyList a -> a
myLast = myHead . myReverse

myNull :: MyList a -> MyBool
myNull Nil = MkTrue
myNull _ = MkFalse

myInit ::  MyList a -> MyList a
myInit = myReverse . myTail . myReverse

myMap :: (a -> b) -> MyList a -> MyList b
myMap f Nil = Nil
myMap f ( x :| xs ) = f x :| myMap f xs

myFst :: MyTuple a b -> a
myFst (MkTuple x y) = x

mySnd :: MyTuple a b -> b
mySnd (MkTuple x y) = y

myLength :: MyList a -> Int
myLength Nil = 0
myLength (x :| xs) =  1 + myLength xs

myFold :: (a -> a -> a) -> a ->  MyList a -> a
myFold f a Nil = a
myFold f a (x :| xs) = x `f` myFold f a xs

myAnd :: MyList Bool -> Bool
myAnd = myFold (&&) True

myAll :: (a -> Bool) -> MyList a -> Bool
myAll p xs = myAnd $ myMap p xs

myOr ::  MyList Bool -> Bool
myOr  = myFold (||) False

myAny :: (a -> Bool) -> MyList a -> Bool
myAny p xs = myOr $ myMap p xs

myElem :: Eq a =>  a ->  MyList a ->  Bool
myElem x xs = myAny (== x) xs

myIntersect :: Eq a => MyList a ->  MyList a ->  MyList a
myIntersect xs ys = myFilter  (`myElem` ys) xs
     

myMapMaybe :: (a -> MyMaybe b) -> MyList a -> MyList b
myMapMaybe f Nil = Nil 
myMapMaybe f (x :| xs) = case f x of
    MkJust y -> y :| ys
    MkNothing -> ys
 where ys = myMapMaybe f xs

myFold1 :: (a -> a -> a) -> MyList a -> a
myFold1 f (x :| Nil) = x
myFold1 f (x :| xs) = x `f` myFold1 f xs

toMyList :: [a] -> MyList a
toMyList  = foldr (:|) Nil

myMaximum :: MyList Int -> Int
myMaximum = myFold1 max

myMinimum :: MyList Int -> Int
myMinimum =  myFold1 min


mySum :: MyList Int -> Int
mySum = myFold (+) 0  
-- mySum Nil = 0
-- mySum (x :| xs) = x + mySum xs

myProduct :: MyList Int -> Int
myProduct = myFold (*) 1
-- myProduct Nil = 1
-- myProduct (x :| xs) = x * myProduct xs

myConcat :: MyList (MyList a) -> MyList a
myConcat = myFold ( +++ ) Nil

myRepeat :: a -> MyList a
myRepeat a = a :| myRepeat a

myReplicate :: Int -> a -> MyList a
myReplicate n a = myTake n $ myRepeat a


myIterate :: ( a -> a ) -> a -> MyList a
myIterate f x = x :| myIterate f (f x) 

-- myEnumFrom :: Int -> MyList Int
-- myEnumFrom a = a :| myEnumFrom (a + 1)

myEnumFrom :: Int -> MyList Int
myEnumFrom a = myIterate (+1) a


myEnumFromTo :: Int ->  Int -> MyList Int
myEnumFromTo n1 n2 
    | n1 > n2 = Nil
    | otherwise = myTake (n2 - n1 + 1) (myEnumFrom n1)
--  where    
-- myEnumFromTo 
-- myEnumFrom a = a :| myEnumFrom (a + 1)

myTake :: Int -> MyList a -> MyList a
myTake n Nil = Nil
myTake n (x :| xs)
    | n <= 0 = Nil 
    | otherwise = x :| myTake (n - 1) xs

myDrop :: Int -> MyList a -> MyList a
myDrop n Nil = Nil
myDrop n (x :| xs)
    | n <= 0 = (x :| xs) 
    | otherwise = myDrop (n - 1) xs

--     | n == 0 = Nil
--     |

myFilter :: (a -> Bool) -> MyList a -> MyList a
myFilter _ Nil = Nil
myFilter p (x :| xs)
    | p x = x :| xs'
    | otherwise = xs'
 where xs' = myFilter p xs 

myZip ::  MyList a ->  MyList b ->  MyList (MyTuple a b)
myZip = myZipWith MkTuple 

-- myZip Nil _ = Nil
-- myZip _ Nil = Nil
-- myZip (x :| xs) (y :| ys) = MkTuple x y :| myZip xs ys

myZipWith ::  (a -> b -> c) -> MyList a ->  MyList b ->  MyList c
myZipWith f  Nil _ = Nil
myZipWith f _ Nil = Nil
myZipWith f (x :| xs) (y :| ys) = f x y :| myZipWith f xs ys


mySolitAt ::  Int -> MyList a -> MyTuple (MyList a) (MyList a)
mySolitAt n xs = MkTuple (myTake n xs) (myDrop n xs)

myTakeWhile :: (a -> Bool) -> MyList a  -> MyList a
myTakeWhile _ Nil =  Nil
myTakeWhile p (x :| xs)
    | p x = x :| myTakeWhile p xs
    | otherwise = Nil

myDropWhile :: (a -> Bool) -> MyList a  -> MyList a
myDropWhile _ Nil =  Nil
myDropWhile p (x :| xs)
    | p x = myDropWhile p xs
    | otherwise = (x :| xs)

mySpan ::  (a -> Bool) -> MyList a  -> MyTuple (MyList a) (MyList a)
mySpan p xs = MkTuple (myTakeWhile p xs) (myDropWhile p xs)


myBreak ::  (a -> Bool) -> MyList a  -> MyTuple (MyList a) (MyList a)
myBreak p = mySpan (not . p) 
-- myBreak p xs = MkTuple (myTakeWhile (not . p) xs) (myDropWhile (not . p) xs)

myIsPrefixOf :: Eq a => MyList a ->  MyList a -> Bool
myIsPrefixOf xs ys
    | myLength xs > myLength ys = False
    | otherwise = myAnd $ myZipWith (==) xs ys

myIsSuffixOf :: Eq a => MyList a ->  MyList a -> Bool
myIsSuffixOf xs ys = myIsPrefixOf (myReverse xs) (myReverse ys) 

myNub :: Eq a => MyList a ->  MyList a
myNub Nil = Nil
myNub (x :| xs) = x :| myFilter (/= x) (myNub xs)

-- uncurry :: (a -> b -> c) -> ((a, b) -> c)
-- curry :: ((a, b) -> c) -> (a -> b -> c)
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)


mVal = do
    x <- MkJust 1
    y <- MkJust 2
    return $ x + y


instance Arbitrary NextThreeDepartureMessage where
    arbitrary = do
        header <- arbitrary :: Gen MessageHeader
        concoursePidInfos <- vectorOf 3 (arbitrary :: Gen ConcoursePidInfo)
        return $ NextThreeDepartureMessage header concoursePidInfos
    shrink = genericShrink

-- data NextThreeDepartureMessage = NextThreeDepartureMessage
--     { header :: MessageHeader
--     , concoursePidInfos :: [ConcoursePidInfo]
--     } deriving (Show, Eq, Generic)

instance Arbitrary a => Arbitrary (MyList a) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary MessageHeader where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary MessageID where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary StationCode where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary RollingStockProfile where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary EquipmentID where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary PlatformNumber where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ConcoursePidInfo where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary LogLine where
    arbitrary = genericArbitrary
    shrink = genericShrink
    
instance Arbitrary Date where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary TimeOfDay where
    arbitrary = do
        hour <- choose (1, 24)
        minute <- choose (1, 60)
        second <- choose (1, 60)
        return $ TimeOfDay hour minute second
        
    shrink = genericShrink

instance Arbitrary Time where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary MyBool where
    arbitrary = genericArbitrary
    shrink = genericShrink
