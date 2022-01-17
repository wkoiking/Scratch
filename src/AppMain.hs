{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

module AppMain where

import GHC.Generics (Generic)

-- GUI関連のモジュール
-- base
import Foreign.Ptr ( castPtr, nullPtr )
import Data.List ( splitAt )
-- sdl2
import qualified SDL as SDL
import qualified SDL.Input.Keyboard.Codes as SDL
-- cairo
import qualified Graphics.Rendering.Cairo as Cairo
-- diagrams-cairo
import Diagrams.Backend.Cairo as Cairo
-- diagrams
import Diagrams.Prelude hiding (view, Vector, (*^), (^+^), (^-^), signorm)
import Diagrams.TwoD.Text (text)

-- base
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newMVar, readMVar, swapMVar, modifyMVar_) -- (newIORef, readIORef, writeIORef, modifyIORef)
import Data.Int (Int32)
import Data.Maybe (listToMaybe, mapMaybe, catMaybes, fromMaybe)

-- 文字列のパーサ
import Data.TrainInformationMessage

-- バイナリデータ(decode）
-- cereal -- serializeからきてる
import Data.Serialize (Get, Putter)
import qualified Data.Serialize as Serial

-- qualified
import Text.Show.Pretty (ppShow)
import Data.List -- drop :: Int -> [a] -> [a]
import Data.Word
import Control.Monad
import Data.Map (Map)
import Data.Tuple (swap)
import qualified Data.Map as M

import Test.QuickCheck hiding (sample, scale)
import Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

-- network
import Network.Socket (Socket, SockAddr)
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as N (recvFrom, sendTo, send)
import Data.IP

import qualified Data.ByteString as B (drop)

type HostName = String
type IPAddr = (Word8, Word8, Word8, Word8)

data SourceID
    = Sys1Net1
    | Sys1Net2
    | Sys2Net1
    | Sys2Net2

sockAddrToSourceID :: N.SockAddr -> Maybe SourceID
sockAddrToSourceID (N.SockAddrInet _ addr) = ipv4AddrToSourceID addr
sockAddrToSourceID (N.SockAddrInet6 _ _ addr _) = ipv6AddrToSourceID addr
sockAddrToSourceID _ = Nothing

refreshScHealthCounter :: Model -> Model
refreshScHealthCounter model = model { scHealthCounterS1N1 = 10
, scHealthCounterS1N2 = 10
, scHealthCounterS2N1 = 10
, scHealthCounterS2N2 = 10
}

updateModelWithHostName :: SourceID -> TrainInformationMessage -> Model -> Model
updateModelWithHostName Sys1Net1 trainInfo model = model { sys1net1 = Just trainInfo }
updateModelWithHostName Sys1Net2 trainInfo model = model { sys1net2 = Just trainInfo } 
updateModelWithHostName Sys2Net1 trainInfo model = model { sys2net1 = Just trainInfo }
updateModelWithHostName Sys2Net2 trainInfo model = model { sys2net2 = Just trainInfo }

ipv4AddrToSourceID :: Word32 -> Maybe SourceID
ipv4AddrToSourceID ipAddr
    | fromHostAddress ipAddr `isMatchedTo` makeAddrRange (toIPv4 [172, 21, 1, 1]) 32 = Just Sys1Net1
    | fromHostAddress ipAddr `isMatchedTo` makeAddrRange (toIPv4 [172, 22, 1, 1]) 32 = Just Sys1Net2
    | fromHostAddress ipAddr `isMatchedTo` makeAddrRange (toIPv4 [172, 21, 1, 2]) 32 = Just Sys2Net1
    | fromHostAddress ipAddr `isMatchedTo` makeAddrRange (toIPv4 [172, 22, 1, 2]) 32 = Just Sys2Net2
    | otherwise = Nothing

ipv6AddrToSourceID :: (Word32, Word32, Word32, Word32) -> Maybe SourceID
ipv6AddrToSourceID ipAddr
    | fromHostAddress6 ipAddr `isMatchedTo` ipv4RangeToIPv6 (makeAddrRange (toIPv4 [172, 21, 1, 1]) 32) = Just Sys1Net1
    | fromHostAddress6 ipAddr `isMatchedTo` ipv4RangeToIPv6 (makeAddrRange (toIPv4 [172, 22, 1, 1]) 32) = Just Sys1Net2
    | fromHostAddress6 ipAddr `isMatchedTo` ipv4RangeToIPv6 (makeAddrRange (toIPv4 [172, 21, 1, 2]) 32) = Just Sys2Net1
    | fromHostAddress6 ipAddr `isMatchedTo` ipv4RangeToIPv6 (makeAddrRange (toIPv4 [172, 22, 1, 2]) 32) = Just Sys2Net2
    | otherwise = Nothing

--UDP受信
mainUdpReceiver :: MVar Model -> IO ()
mainUdpReceiver vModel = do
    sock <- openSockUdpReceiver "55135"　-- ポート番号のみ(受ける側) 55835…SC801
    let loop = do
            (bstr, addr) <- N.recvFrom sock 1472 -- イーサネットの最大フレーム長　(UDPの場合、イーサネットのフレーム長を超えるとパソケットがばらける) 
            -- ブロッキング状態　：　何も受信しなければ処理が止まる(タイムアウトさせるか、手動で終わらせる)
            -- (B.drop 76 bstr)　受信したデータから76byte捨てる
            case Serial.runGet getTrainInformationMessage (B.drop 76 bstr) :: Either String TrainInformationMessage of
                Right (TrainInformationMessage common trainInfo) -> do
--                     putStrLn $ "受信しました"
--                     putStrLn $ ppShow trainInfo
                    forM_ (sockAddrToSourceID addr) $ \ srcID -> do
                        modifyMVarPure_ vModel $ refreshScHealthCounter . updateModelWithHostName srcID (TrainInformationMessage common (filter isValidRakeID trainInfo))
                    model <- readMVar vModel
                    putStrLn $ ppShow model
                    loop
                Left err -> do
--                     putStrLn err
--                     putStrLn "Exitting"
                    N.close sock
    loop

openSockUdpReceiver
    :: String -- ^ Port number or name
    -> IO Socket
openSockUdpReceiver port = do
    addrinfos <- N.getAddrInfo (Just (N.defaultHints {N.addrFlags = [N.AI_PASSIVE]})) Nothing (Just port)
    let serveraddr = head addrinfos
    sock <- N.socket (N.addrFamily serveraddr) N.Datagram N.defaultProtocol
    N.bind sock (N.addrAddress serveraddr)
    return sock

----------------------------------------
-- Model, view, update
----------------------------------------

data Model = MkModel
    { selectedRakeID :: Maybe RakeID
    , sys1net1 :: Maybe TrainInformationMessage
    , sys1net2 :: Maybe TrainInformationMessage
    , sys2net1 :: Maybe TrainInformationMessage
    , sys2net2 :: Maybe TrainInformationMessage
    , scHealthCounterS1N1 :: Int
    , scHealthCounterS1N2 :: Int
    , scHealthCounterS2N1 :: Int
    , scHealthCounterS2N2 :: Int
    } deriving (Show)

initialModel :: Model
initialModel = MkModel
    { selectedRakeID      = Nothing
    , sys1net1            = Nothing
    , sys1net2            = Nothing
    , sys2net1            = Nothing
    , sys2net2            = Nothing
    , scHealthCounterS1N1 = -1
    , scHealthCounterS1N2 = -1
    , scHealthCounterS2N1 = -1
    , scHealthCounterS2N2 = -1 
    }

view :: Model -> SelectableDiagram
view MkModel{..} = scale 1.5 $ bg2 bgCol $ center $ hcat $ map alignT [trainListDiagram, value [] $ trainInformationsDiagram]
 where bgCol :: Colour Double
       bgCol
           | scHealthCounterS1N1 < 0 = red
           | scHealthCounterS1N2 < 0 = red
           | scHealthCounterS2N1 < 0 = red
           | scHealthCounterS2N2 < 0 = red
           | otherwise = white
       trainListDiagram :: SelectableDiagram
       trainListDiagram = viewTrainList selectedRakeID rakes
       rakes :: [RakeID]
       rakes = nub $ concat $ map trainInformationMessageToRakes $ catMaybes [sys1net1, sys1net2, sys2net1, sys2net2]
       trainInformationMessageToRakes :: TrainInformationMessage -> [RakeID]
       trainInformationMessageToRakes message = map fst $ trainPart message
       trainInformationsDiagram :: NormalDiagram
       trainInformationsDiagram = fromMaybe mempty $ do
           rakeID <- selectedRakeID
           return $ center $ hcat
                  $ mapMaybe (viewTrainInformation' rakeID) [("sys1net1", sys1net1), ("sys1net2", sys1net2), ("sys2net1", sys2net1), ("sys2net2", sys2net2)]
            where viewTrainInformation' :: RakeID -> (String, Maybe TrainInformationMessage) -> Maybe NormalDiagram
                  viewTrainInformation' rakeID (title, mTrainInfomationMessage) = do
                      trainInfoMsg <- mTrainInfomationMessage
                      trainInfo <- lookup rakeID $ trainPart trainInfoMsg
                      return $ viewTrainInformation rakeID (title, trainInfo)
                  

--        trainInformationsDiagram = fromMaybe mempty $ do
--            rakeID <- selectedRakeID
--            TrainInformationMessage _ sys1net1Trains <- sys1net1
--            TrainInformationMessage _ sys1net2Trains <- sys1net2
--            TrainInformationMessage _ sys2net1Trains <- sys2net1
--            TrainInformationMessage _ sys2net2Trains <- sys2net2
--            tr1 <- lookup rakeID sys1net1Trains
--            tr2 <- lookup rakeID sys1net2Trains
--            tr3 <- lookup rakeID sys2net1Trains
--            tr4 <- lookup rakeID sys2net2Trains
--            return $ hcat $ map (viewTrainInformation rakeID) [tr1, tr2, tr3, tr4]



viewTrainInformation :: RakeID -> (String, TrainInformation) -> NormalDiagram
viewTrainInformation rakeID (title, TrainInformation{..}) = vcat [simpleTextBox aquamarine title, textBoxes, bitBoxes]
 where textBoxes = center $ vsep 0.3
           [ textBox "OCCRemoteCommandAck" $ show statOCCRemoteCommandAck
           , textBox "FrontBlock" $ show $ frontBlockID statTrainLocationBlock
           , textBox "FrontOffset" $ show $ frontOffset statTrainLocationBlock
           , textBox "RearBlock" $ show $ rearBlockID statTrainLocationBlock
           , textBox "RearOffset" $ show $ rearOffset statTrainLocationBlock
           , textBox "TrainSpeed" $ show statTrainSpeed 
           , textBox "DrivingMode" $ maybe "NA" show statDrivingMode
           , textBox "DistanceToMA" $ show statDistanceToMA 
           , textBox "RollingStockProfile" $ show statRollingStockProfile 
           , textBox "InitStatus" $ show statInitStatus
           , textBox "Direction" $ showDirection statRunningDirection
           , textBox "Sleep Mode" $ maybe "NA" show statSleepMode
           , textBox "Regime" $ maybe "NA" show statRegime
           , textBox "DrivingStatus" $ maybe "NA" show statDrivingStatus
           , textBox "AdditionalInfo" $ show statAdditionalInfo
           , textBox "EBReasonVOBC" $ show statEBReasonVOBC
           , textBox "EBReasonSC" $ show statEBReasonSC
           , textBox "OnBoardFailure" $ show statOnBoardATCFailureInformation
           ]
       bitBoxes = center $ vcat $ map hcat $ toTable 3
           [ bitBox "TrainReady" statTrainReady 
           , bitBox "Departure" statDeparture
           , bitBox "SkipStop" statSkipStop
           , bitBox "TrainStopped" statTrainStopped
           , bitBox "TestTrack" statTestTrack
           , bitBox "Passed" statPassed
           , bitBox "Overspeed" statOverspeed
           , bitBox "DoorEnabled" statDoorEnabled
           , bitBox "EBReleaseAck" statEBReleaseAck
           , bitBox "EBReleaseCmd" statEBReleaseCommand 
           , bitBox "P0Stopped" statP0Stopped 
           , bitBox "Held" statHeld
           , bitBox "Removed" statRemoved
           , bitBox "System" statSystem
           , bitBox "WakeupAck" statWakeupAck
           , bitBox "StanbyAck" statStanbyAck
           , bitBox "DM1 Master" $ fst statMasterSlave
           , bitBox "DM2 Master" $ snd statMasterSlave
           , bitBox "DM1 Reset" $ fst statResetResult 
           , bitBox "DM2 Reset" $ snd statResetResult 
           , bitBox "LDoorClosed" $ fst statDoorClosedStatus 
           , bitBox "RDoorClosed" $ snd statDoorClosedStatus
           , bitBox "RescueTrain" statIsRescueTrain
           ]

toTable :: Int -> [a] -> [[a]]
toTable n [] = []
toTable n ds = ls : toTable n rs
 where (ls, rs) = splitAt n ds

showDirection :: (Bool, Bool) -> String
showDirection (True, False) = "<=="
showDirection (False, True) = "==>"
showDirection _ = "Invalid"

viewTrainList :: Maybe RakeID -> [RakeID] -> SelectableDiagram
viewTrainList mSelectedRake rakes = vcat $ map viewTrain rakes
 where viewTrain :: RakeID -> SelectableDiagram
       viewTrain rakeID = value [SelectRake rakeID] $ simpleTextBox bgCol $ show rakeID
        where bgCol 
                  | mSelectedRake == Just rakeID = skyblue
                  | otherwise = white

simpleTextBox :: Colour Double -> String -> NormalDiagram
simpleTextBox bgCol contents = mconcat
    [ text contents # fc black # lw none
    , rect 8 2 # fc bgCol # lc black
    ]

textBox
    :: String -- ^ タイトル
    -> String -- ^ 中身
    -> NormalDiagram
textBox title contents = hsep 0.5 [titleDiagram, contentDiagram]
 where titleDiagram = mconcat
           [ text title # fc black # lw none
           , phantom r -- phantomを使うときは引数の型を指定する必要有
           ]
       contentDiagram = mconcat
           [ text contents # fc black # lw none
           , r # fc white # lc black
           ]
       r :: NormalDiagram
       r = rect 12 2

bg2 :: Colour Double -> SelectableDiagram -> SelectableDiagram
bg2 col d = d <> background
 where background = boundingRect (clearValue d) # lw none # fc col # value []

bitBox
    :: String -- ^ タイトル
    -> Bool
    -> NormalDiagram
bitBox title bit = mconcat
    [ text title # fc black # lw none
    , rect 10 3 # fc col # lw none
    ]
 where col | bit = yellow
           | otherwise = gray
              
updateWithTimer :: (SDL.Scancode -> Bool) -> Model -> Model
updateWithTimer isPressed model = model { scHealthCounterS1N1 = scHealthCounterS1N1 model - 1
, scHealthCounterS1N2 = scHealthCounterS1N2 model - 1
, scHealthCounterS2N1 = scHealthCounterS2N1 model - 1
, scHealthCounterS2N2 = scHealthCounterS2N2 model - 1
}

updateWithClick :: Button -> Model -> Model
updateWithClick (SelectRake rakeID) model = model {selectedRakeID = Just rakeID}
-- updateWithClick (SelectRake rakeID) (MkModel _ sys1net1 sys1net2 sys2net1 sys2net2) = MkModel (Just rakeID) sys1net1 sys1net2 sys2net1 sys2net2

data Button
    = SelectRake RakeID

updateWithKeyPress :: SDL.Keycode -> Model -> Model
updateWithKeyPress _ model = model

----------------------------------------
-- GUIのあれこれ
----------------------------------------

type NormalDiagram = Diagram V2

type GenericDiagram a = QDiagram V2 Double a

type SelectableDiagram = GenericDiagram [Button]

-- rasterize :: SizeSpec V2 Int -> Diagram V2 -> Diagram V2
-- rasterize sz d = sizedAs d $ imageEmb $ ImageRGBA8 $ renderImage sz d

modifyMVarPure_ :: MVar a -> (a -> a) -> IO ()
modifyMVarPure_ var f = modifyMVar_  var $ return . f

value :: Monoid m => m -> QDiagram v n Any -> QDiagram v n m
value m = fmap fromAny
  where fromAny (Any True)  = m
        fromAny (Any False) = mempty

resetValue :: (Eq m, Monoid m) => QDiagram v n m -> QDiagram v n Any
resetValue = fmap toAny
  where toAny m | m == mempty = Any False
                | otherwise   = Any True

clearValue :: QDiagram v n m -> QDiagram v n Any
clearValue = fmap (const (Any False))

fullHDRect :: NormalDiagram
fullHDRect = rect screenWidth screenHeight # fc white

screenWidth :: Num a => a
screenWidth = 1920
screenHeight :: Num a => a
screenHeight = 1080

mainApp :: IO ()
mainApp = do
    -- 編集の初期化
    vModel <- newMVar initialModel
    vRender <- newMVar $ view initialModel
    forkIO $ mainUdpReceiver vModel

    -- SDL初期化
    SDL.initialize [ SDL.InitVideo ]
    window <- SDL.createWindow
        "SDL / Cairo Example"
        SDL.defaultWindow {SDL.windowInitialSize = SDL.V2 screenWidth screenHeight}
    SDL.showWindow window
    
    screenSdlSurface <- SDL.getWindowSurface window

    sdlSurface <- SDL.createRGBSurface (SDL.V2 screenWidth screenHeight) SDL.ARGB8888
    buffer <- fmap castPtr $ SDL.surfacePixels sdlSurface
    cairoSurface <- Cairo.createImageSurfaceForData buffer Cairo.FormatRGB24 screenWidth screenHeight (screenWidth * 4)

    SDL.updateWindowSurface window

    -- Userイベントの登録
    mRegisteredEventType <- SDL.registerEvent decodeUserEvent encodeUserEvent
    let pushCustomEvent :: CustomEvent -> IO ()
        pushCustomEvent userEvent = forM_ mRegisteredEventType $ \ regEventType -> SDL.pushRegisteredEvent regEventType userEvent
        getCustomEvent :: SDL.Event -> IO (Maybe CustomEvent)
        getCustomEvent event = case mRegisteredEventType of
            Nothing -> return $ Nothing
            Just regEventType -> SDL.getRegisteredEvent regEventType event

    -- 定周期の処理
    let cycleInterval = 500 -- 500msec
    _ <- SDL.addTimer cycleInterval $ const $ do
        isPressed <- SDL.getKeyboardState
        modifyMVarPure_ vModel $ updateWithTimer isPressed
        pushCustomEvent CustomExposeEvent
        return $ SDL.Reschedule cycleInterval 

    pushCustomEvent CustomExposeEvent

    -- Eventハンドラ
    let loop :: IO ()
        loop = do
            event <- SDL.waitEvent
            mUserEvent <- getCustomEvent event
            forM_ mUserEvent $ \case
                CustomExposeEvent -> do
                    model <- readMVar vModel
--                     putStrLn $ show $ triangleClickCount model
                    let selectableDiagram :: SelectableDiagram
                        selectableDiagram = toSDLCoord $ scale 10 $ view model

                    SDL.surfaceFillRect sdlSurface Nothing whiteRect
                    Cairo.renderWith cairoSurface $ Cairo.toRender mempty $ clearValue selectableDiagram
                    SDL.surfaceBlit sdlSurface Nothing screenSdlSurface Nothing

                    SDL.updateWindowSurface window
                    swapMVar vRender selectableDiagram
                    return ()
            case SDL.eventPayload event of
                SDL.MouseButtonEvent SDL.MouseButtonEventData{..} -> do
                    case mouseButtonEventMotion of
                        SDL.Pressed -> do
                            selectableDiagram <- readMVar vRender
                            let mClickedObj = listToMaybe $ reverse $ sample selectableDiagram $ toFloatingPoint $ mouseButtonEventPos
                            case mClickedObj of
                                Nothing -> return ()
                                Just obj -> modifyMVarPure_ vModel $ updateWithClick obj
                            pushCustomEvent CustomExposeEvent
                            loop
                        _           -> loop
                SDL.KeyboardEvent SDL.KeyboardEventData{..} | keyboardEventKeyMotion == SDL.Pressed -> do
                    let SDL.Keysym _ key SDL.KeyModifier{..} = keyboardEventKeysym
                    modifyMVarPure_ vModel $ updateWithKeyPress key
                    pushCustomEvent CustomExposeEvent
                    loop
                SDL.QuitEvent       -> return ()
                _                   -> loop
    loop
    putStrLn "Exitting"

data CustomEvent = CustomExposeEvent

decodeUserEvent :: SDL.RegisteredEventData -> SDL.Timestamp -> IO (Maybe CustomEvent)
decodeUserEvent SDL.RegisteredEventData{..} _ = case registeredEventCode of
    0 -> return $ Just CustomExposeEvent
    _ -> return Nothing

encodeUserEvent :: CustomEvent -> IO SDL.RegisteredEventData
encodeUserEvent CustomExposeEvent = return $ SDL.RegisteredEventData Nothing 0 nullPtr nullPtr

toSDLCoord :: SelectableDiagram -> SelectableDiagram
toSDLCoord = translate (V2 (screenWidth / 2) (screenHeight / 2)) . reflectY

toFloatingPoint :: Point V2 Int32 -> Point V2 Double
toFloatingPoint p = fmap fromIntegral p

whiteRect :: SDL.V4 Word8
whiteRect = SDL.V4 maxBound maxBound maxBound maxBound

alphaRect :: SDL.V4 Word8
alphaRect = SDL.V4 maxBound maxBound maxBound minBound
