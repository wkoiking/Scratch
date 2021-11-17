module Main where

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
import  Data.Bits.Coding as Bit
-- バイナリデータ(encode）


-- qualified
import Text.Show.Pretty (ppShow)
import Data.List
import Data.Word
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as M

main = do
    str <- readFile "C:/Users/dmrc/Desktop/scratch/haskell-from-scratch/app/2021-10-04-logfile"
    case P.parse parseLogFile "" str of
        Right loglines -> do
            putStrLn $ ppShow $ length $ filter (("ScReceiver" `isInfixOf`) . label)  loglines 
        Left err -> do
            putStrLn $ show err

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
    deriving (Show)

data MyMaybe a where
    MkJust :: a -> MyMaybe a
    MkNothing :: MyMaybe a
    deriving (Show)



data MyEither a b where
    MkLeft :: a -> MyEither a b
    MkRight :: b -> MyEither a b
    deriving (Show)

data MyList a where
    (:|) :: a -> MyList a -> MyList a
    Nil :: MyList a
    deriving (Show)


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
mapDecodeStationCode = M.fromList $ zip [31 ..] [JPW ..]

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
putTimeOfDay = undefined

putNextThreeDepartureMessage :: Putter NextThreeDepartureMessage
putNextThreeDepartureMessage (NextThreeDepartureMessage header concoursePidInfos ) = do
    putMessageHeader header
    mapM_ putConcoursePidInfo concoursePidInfos

putMessageHeader :: Putter MessageHeader
putMessageHeader (MessageHeader messageSorce messageID messageStationCode) = undefined

    


putMessageID :: Putter MessageID
putMessageID messageID = Serial.putWord8 $ case messageID of
       NextThreeDeparture -> 2
       DeparturePlatfrom  -> 3 
       ArrivalPlatfrom -> 4 
       
putStationCode :: Putter StationCode
putStationCode stationCode = undefined
        -- これ、部分関数になってますか…？


-- getStationCode :: Get StationCode
-- getStationCode = do
--     n <- Serial.getWord8
--     case M.lookup n mapDecodeStationCode of
--         Just stCode -> return stCode
--         Nothing -> fail"undefined getStationCode"



putEquipmentID :: Putter EquipmentID
putEquipmentID equipmentID = Serial.putWord8 $ case equipmentID of
    ATS -> 1
    PID -> 2

putRollingStockProfile :: Putter (Maybe RollingStockProfile)
putRollingStockProfile = undefined

putPlatformNumber :: Putter PlatformNumber
putPlatformNumber = undefined

putConcoursePidInfo :: Putter ConcoursePidInfo
putConcoursePidInfo = undefined 



data NextThreeDepartureMessage = NextThreeDepartureMessage
    { header :: MessageHeader
    , concoursePidInfos :: [ConcoursePidInfo]
    } deriving (Show)

data MessageHeader = MessageHeader
    { messageSource :: EquipmentID 
    , messageID :: MessageID
    , messageStationCode :: StationCode
    } deriving (Show)

data MessageID
   = NextThreeDeparture
   | ArrivalPlatfrom
   | DeparturePlatfrom
   deriving (Show)

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
    deriving (Show, Enum, Bounded)

data RollingStockProfile
    = FourCar
    | SixCar
    | EightCar
    deriving (Show)

data EquipmentID = ATS | PID deriving (Show)
    
data PlatformNumber = PlatformNumber
    { unPlatformNumber :: Word8
    } deriving (Show)

data ConcoursePidInfo = ConcoursePidInfo
    { pidRollingStockProfile :: Maybe RollingStockProfile
    , pidPlatformNumber :: PlatformNumber
    , pidTimeOfDay :: TimeOfDay
    , pidDestinationStation :: StationCode
    } deriving (Show)
    

data LogLine = LogLine
    { timeStamp :: Time
    , label :: String
    , message :: String
    } deriving (Show)

data Date = Date
    { year :: Int
    , month :: Int
    , day ::  Int
    } deriving (Show)

data TimeOfDay = TimeOfDay
    { hour :: Int
    , minute :: Int
    , second ::  Int
    } deriving (Show)

data Time = Time
    { date :: Date
    , timeOfDay :: TimeOfDay
    } deriving (Show)


is9oClock :: LogLine -> Bool
is9oClock logline  = hour (timeOfDay $ timeStamp $ logline) == 9






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
    deriving (Show)

instance Eq MyBool where
    MkTrue == MkTrue = True
    MkFalse == MkFalse = True
    _ == _ = False

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
