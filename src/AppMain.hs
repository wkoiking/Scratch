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
import qualified SDL.Raw.Event as SDLE (startTextInput)

-- cairo
import qualified Graphics.Rendering.Cairo as Cairo
-- diagrams-cairo
import Diagrams.Backend.Cairo as Cairo
-- diagrams
import Diagrams.Prelude hiding (view, Vector, (*^), (^+^), (^-^), signorm)
import Diagrams.TwoD.Text (text)

-- text
import qualified Data.Text as T (unpack) -- Text -> String

-- base
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newMVar, readMVar, swapMVar, modifyMVar_) -- (newIORef, readIORef, writeIORef, modifyIORef)
import Data.Int (Int32)
import Data.Maybe (listToMaybe, mapMaybe, catMaybes, fromMaybe)

-- directory
import System.Directory (listDirectory)

-- filepath
import System.FilePath (takeExtension, (</>))

-- parsec
import qualified Text.Parsec as P

-- bytestring

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

-- 文字列のパーサ
-- import Data.TrainInformationMessage
import Data.PasreAssignmentFileOC

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

import Test.QuickCheck hiding (sample, scale, sized)
import Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

-- lens
import Control.Lens hiding ((#), view, none)

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
    deriving (Show, Eq, Ord, Enum, Bounded)

sockAddrToSourceID :: N.SockAddr -> Maybe SourceID
sockAddrToSourceID (N.SockAddrInet _ addr) = ipv4AddrToSourceID addr
sockAddrToSourceID (N.SockAddrInet6 _ _ addr _) = ipv6AddrToSourceID addr
sockAddrToSourceID _ = Nothing

refreshScHealthCounter :: Model -> Model
refreshScHealthCounter model = model { scHealthCounter = 10 }

updateModelWithHostName :: SourceID -> Map SheetName [(String, Bool)] -> Model -> Model
updateModelWithHostName Sys1Net1 ocStatus model = model { sys1net1 = sys1net1 model `M.union` ocStatus }
updateModelWithHostName Sys1Net2 ocStatus model = model { sys1net2 = sys1net2 model `M.union` ocStatus } 
updateModelWithHostName Sys2Net1 ocStatus model = model { sys2net1 = sys2net1 model `M.union` ocStatus }
updateModelWithHostName Sys2Net2 ocStatus model = model { sys2net2 = sys2net2 model `M.union` ocStatus }

ipv4AddrToSourceID :: Word32 -> Maybe SourceID
ipv4AddrToSourceID ipAddr
    | fromHostAddress ipAddr `isMatchedTo` makeAddrRange (toIPv4 [172, 21, 51, 1]) 32 = Just Sys1Net1
    | fromHostAddress ipAddr `isMatchedTo` makeAddrRange (toIPv4 [172, 22, 51, 1]) 32 = Just Sys1Net2
    | fromHostAddress ipAddr `isMatchedTo` makeAddrRange (toIPv4 [172, 21, 51, 2]) 32 = Just Sys2Net1
    | fromHostAddress ipAddr `isMatchedTo` makeAddrRange (toIPv4 [172, 22, 51, 2]) 32 = Just Sys2Net2
    | otherwise = Nothing

ipv6AddrToSourceID :: (Word32, Word32, Word32, Word32) -> Maybe SourceID
ipv6AddrToSourceID ipAddr
    | fromHostAddress6 ipAddr `isMatchedTo` ipv4RangeToIPv6 (makeAddrRange (toIPv4 [172, 21, 51, 1]) 32) = Just Sys1Net1
    | fromHostAddress6 ipAddr `isMatchedTo` ipv4RangeToIPv6 (makeAddrRange (toIPv4 [172, 22, 51, 1]) 32) = Just Sys1Net2
    | fromHostAddress6 ipAddr `isMatchedTo` ipv4RangeToIPv6 (makeAddrRange (toIPv4 [172, 21, 51, 2]) 32) = Just Sys2Net1
    | fromHostAddress6 ipAddr `isMatchedTo` ipv4RangeToIPv6 (makeAddrRange (toIPv4 [172, 22, 51, 2]) 32) = Just Sys2Net2
    | otherwise = Nothing

--UDP受信
mainUdpReceiver :: String -> Get (Map SheetName ByteString) -> [(String, SheetName, Int)] -> MVar Model -> IO ()
mainUdpReceiver port getOC2ATS assignments vModel = do
    sock <- openSockUdpReceiver port -- ポート番号のみ(受ける側) 55835…SC801
    let loop = do
            (bstr, addr) <- N.recvFrom sock 1472 -- イーサネットの最大フレーム長　(UDPの場合、イーサネットのフレーム長を超えるとパソケットがばらける)
            putStrLn $ "received:" ++ port
            -- ブロッキング状態　：　何も受信しなければ処理が止まる(タイムアウトさせるか、手動で終わらせる)
            -- (B.drop 76 bstr)　受信したデータから76byte捨てる
            case Serial.runGet getOC2ATS bstr :: Either String (Map SheetName ByteString) of
                Right sheets -> do
--                     putStrLn $ "受信しました"
--                     putStrLn $ ppShow trainInfo
                    forM_ (sockAddrToSourceID addr) $ \ srcID -> do
--                         putStrLn $ show $ toBitStatusOC2ATS1 assignments sheets
                        modifyMVarPure_ vModel $ refreshScHealthCounter . updateModelWithHostName srcID (toBitStatus assignments sheets)
                    model <- readMVar vModel
--                     putStrLn $ ppShow model
                    loop
                Left err -> do
                    putStrLn err
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
    { selectedSheet :: SheetName
    , scrollCount :: Int
    , sys1net1 :: Map SheetName [(String, Bool)]
    , sys1net2 :: Map SheetName [(String, Bool)]
    , sys2net1 :: Map SheetName [(String, Bool)]
    , sys2net2 :: Map SheetName [(String, Bool)]
    , scHealthCounter :: Int
    , searchString :: String -- 検索用
    } deriving (Show)

initialModel :: Model
initialModel = MkModel
    { selectedSheet   = I1
    , scrollCount     = 0
    , sys1net1        = mempty
    , sys1net2        = mempty
    , sys2net1        = mempty
    , sys2net2        = mempty
    , scHealthCounter = -1
    , searchString     = ""
    }

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay xs = Just $ maximum xs

view :: Model -> SelectableDiagram
view MkModel{..} = center $ mconcat $ map alignT
    [ contents # sizedAs screenRect
    , value [] $ screenRect
    ]
 where contents = center $ vcat
           [ sheetList 
           , center $ hcat [sheetDiagram, scrollBar]
           ]
       scrollBar :: SelectableDiagram
       scrollBar = value [] $ sized (mkHeight h) $ hcat [viewScrollBar scrollCount allBitsCount, simpleTextBox white searchString]
       h :: Double
       h = screenHeight - height sheetList
       allBitsCount :: Int
       allBitsCount = fromMaybe 0 $ maximumMay $ map length $ catMaybes [bits1_1, bits1_2, bits2_1, bits2_2]
      
       sheetDiagram = value [] $ sized (mkHeight h) $ viewSheet selectedBits
       sheetList :: SelectableDiagram
       sheetList = sized (mkWidth $ width screenRect) $ viewSheetList selectedSheet [I1 ..]

       bits1_1 = M.lookup selectedSheet sys1net1 
       bits1_2 = M.lookup selectedSheet sys1net2
       bits2_1 = M.lookup selectedSheet sys2net1
       bits2_2 = M.lookup selectedSheet sys2net2

       selectedBits :: [(SourceID, [(String, Bool)])]
       selectedBits = map scroll $ catMaybes $ zipWith addSourceID [Sys1Net1 ..] [bits1_1, bits1_2, bits2_1, bits2_2]
        where addSourceID :: SourceID -> Maybe [(String, Bool)] -> Maybe (SourceID, [(String, Bool)])
              addSourceID srcID mbits = fmap (srcID ,) mbits
              scroll :: (SourceID, [(String, Bool)]) -> (SourceID, [(String, Bool)])
              scroll (srcID, bs) = ( srcID,  take numOfBitsVertical $  drop (scrollCount * numOfBitsVertical ) bs )
--                fmap (1 +) (Just 1) ==> Just (1 + 1) ==> Just 2
--                fmap (1 ,) (Just 1) ==> Just (1 , 1)
       bgCol :: Colour Double
       bgCol
           | scHealthCounter < 0 = red
           | otherwise = white
       screenRect :: NormalDiagram
       screenRect = rect screenWidth screenHeight # bg bgCol

viewSheet :: [(SourceID, [(String, Bool)])] -> NormalDiagram
viewSheet bss = center $ hcat $ map viewOneSource bss
 where viewOneSource :: (SourceID, [(String, Bool)]) -> NormalDiagram
       viewOneSource (srcID, bs) = vcat [title ,bitBoxs]
        where title :: NormalDiagram
              title = colorBox (show srcID) mintcream
              bitBoxs :: NormalDiagram
              bitBoxs = vcat $ take numOfBitsVertical $ map (uncurry bitBox) bs ++ repeat (colorBox "" white)

viewScrollBar :: Int -> Int -> NormalDiagram
viewScrollBar scrollCount allBitsCount
    | allBitsCount <= numOfBitsVertical = scrollBarBgRect
    | otherwise = center $ mconcat
        [ translateY (- vShift) $ alignT thumb
        , alignT scrollBarBgRect
        ]
 where thumb :: NormalDiagram
       thumb = rect scrollBarWidth thumbHeight # fc gray
       thumbHeight :: Double
       thumbHeight = fromIntegral numOfBitsVertical / fromIntegral allBitsCount * scrollBarHeight
       vShift :: Double
       vShift = fromIntegral (scrollCount * numOfBitsVertical) / fromIntegral allBitsCount * scrollBarHeight
       

scrollBarBgRect :: NormalDiagram
scrollBarBgRect = rect scrollBarWidth scrollBarHeight # fc lightgray
scrollBarWidth :: Double
scrollBarWidth = 1
scrollBarHeight :: Double
scrollBarHeight = 50

numOfBitsVertical :: Int
numOfBitsVertical = 40


toTable :: Int -> [a] -> [[a]]
toTable n [] = []
toTable n ds = ls : toTable n rs
 where (ls, rs) = splitAt n ds

showDirection :: (Bool, Bool) -> String
showDirection (True, False) = "<=="
showDirection (False, True) = "==>"
showDirection _ = "Invalid"

viewSheetList :: SheetName -> [SheetName] -> SelectableDiagram
viewSheetList selectedSheet sheets = center $ hcat $ map viewSheet sheets
 where viewSheet :: SheetName -> SelectableDiagram
       viewSheet sheet = value [SelectSheet sheet] $ simpleTextBox bgCol $ show sheet
        where bgCol 
                  | selectedSheet == sheet = skyblue
                  | otherwise = white

simpleTextBox :: Colour Double -> String -> NormalDiagram
simpleTextBox bgCol contents = mconcat
    [ text contents # fc black # lw none
    , rect 9 2 # fc bgCol # lc black
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
bitBox title bit = colorBox title col
 where col | bit = yellow
           | otherwise = gray

colorBox
    :: String -- ^ タイトル
    -> Colour Double
    -> NormalDiagram
colorBox title col = mconcat
    [ scale 0.5 $ text title # fc black # lw none
    , rect 9 1.5 # fc col # lw veryThin # lc black
    ]

updateWithTimer :: (SDL.Scancode -> Bool) -> Model -> Model
updateWithTimer isPressed model = model { scHealthCounter = scHealthCounter model - 1 }

updateWithClick :: Button -> Model -> Model
updateWithClick (SelectSheet sheet) model = model {selectedSheet = sheet}
-- updateWithClick (SelectRake rakeID) (MkModel _ sys1net1 sys1net2 sys2net1 sys2net2) = MkModel (Just rakeID) sys1net1 sys1net2 sys2net1 sys2net2

data Button
    = SelectSheet SheetName

updateWithKeyPress :: SDL.Keycode -> Model -> Model
updateWithKeyPress SDL.KeycodeUp model = model { scrollCount = max 0 ( scrollCount model - 1 ) }
updateWithKeyPress SDL.KeycodeDown model = model { scrollCount = scrollCount model + 1 }
updateWithKeyPress SDL.KeycodeRight model = model { scrollCount = 0, selectedSheet = nectSheet $ selectedSheet model }
updateWithKeyPress SDL.KeycodeLeft model = model { scrollCount = 0, selectedSheet = previounsSheet $ selectedSheet model }
updateWithKeyPress _ model = model

nectSheet :: SheetName -> SheetName
nectSheet Alt = Alt 
nectSheet sheet = succ sheet

previounsSheet :: SheetName -> SheetName
previounsSheet I1 = I1
previounsSheet sheet = pred sheet

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

csvFileLocation :: FilePath
csvFileLocation = "C:/Users/haske/Desktop/haskell/OC-purser"
-- CSVファイルはリストで

mainApp :: IO ()
mainApp = do
    -- 編集の初期化
    files <- listDirectory csvFileLocation
    Just filename <- return $ find ((".csv" ==) . takeExtension) files
    str <- readFile $ csvFileLocation </> filename
    Right assignments <- return $ (P.parse parseAssignmentFile "" str :: Either P.ParseError [(String, SheetName, Int)])
    vModel <- newMVar initialModel
    vRender <- newMVar $ view initialModel
    forkIO $ mainUdpReceiver "58198" getOC2ATS1 assignments vModel 
    forkIO $ mainUdpReceiver "58197" getOC2ATS2 assignments vModel
    forkIO $ mainUdpReceiver "58196" getOC2ATS3 assignments vModel


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
                        selectableDiagram = toSDLCoord $ view model

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

                SDL.MouseWheelEvent SDL.MouseWheelEventData{..} -> do
                    modifyMVarPure_ vModel $ if
                        | y > 0     -> updateWithKeyPress SDL.KeycodeUp
                        | y < 0     -> updateWithKeyPress SDL.KeycodeDown
                        | otherwise -> id
                    pushCustomEvent CustomExposeEvent
                    loop
                 where V2 _ y = mouseWheelEventPos
                SDL.KeyboardEvent SDL.KeyboardEventData{..} | keyboardEventKeyMotion == SDL.Pressed -> do
                    let SDL.Keysym _ key SDL.KeyModifier{..} = keyboardEventKeysym
                    modifyMVarPure_ vModel $ updateWithKeyPress key
                    let updateByBackspace :: Model -> Model
                        updateByBackspace model = model { searchString = take (length str - 1) str}
                         where str = searchString model
                    when (SDL.KeycodeBackspace == key) $ modifyMVarPure_ vModel updateByBackspace

                    pushCustomEvent CustomExposeEvent
                    loop
--                     if key == SDL.KeycodeQ
--                         then return ()
--                         else loop
                SDL.TextInputEvent SDL.TextInputEventData{..} -> do
                    let updateByTextInput :: Model -> Model
                        updateByTextInput model = model { searchString = searchString model ++ T.unpack textInputEventText }
                    modifyMVarPure_ vModel updateByTextInput
                    pushCustomEvent CustomExposeEvent
                    loop
                SDL.QuitEvent       -> return ()
                _                   -> loop
    SDLE.startTextInput
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

$(makeLenses  ''Model)