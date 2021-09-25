{-# LANGUAGE CPP #-}

{-|
Module      : Mane
Description : VELDT FPGA Programmer
Copyright   : (c) David Cox, 2021
License     : MIT
Maintainer  : standardsemiconductor@gmail.com
-}

module Mane 
  ( findFPGADevice
  , printJedecID
  , toggleReset
  , progFlash
  , ManeConfig(..)
  , ManeFailure(..)
  , deviceInfo
  ) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Control.Concurrent (threadDelay)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Bits
import Data.List (find)
import qualified Data.Vector as V (toList)
import Data.Word
import Numeric (showHex)
import System.FTDI
import System.FTDI.MPSSE
import System.IO
import qualified System.USB as USB
import Text.Printf (printf)

----------------
-- Mane Monad --
----------------
-- | Mane configuration
data ManeConfig = ManeConfig
  { vendorId  :: USB.VendorId  -- ^ FTDI Vendor ID
  , productId :: USB.ProductId -- ^ FTDI Product ID
  }

-- | Failure data type
data ManeFailure = FPGANotFound -- ^ Cannot find FPGA device with vendor ID and product ID
                 | FTDIFailure Failure -- ^ FTDI Failure from ftdi library
  deriving Show

newtype Mane a = Mane { unMane :: ReaderT ManeConfig (ExceptT ManeFailure IO) a }
  deriving (Functor, Applicative, Monad
           , MonadReader ManeConfig
           , MonadError ManeFailure
           , MonadIO
           )

runMane :: ManeConfig -> Mane a -> IO (Either ManeFailure a)
runMane cfg = runExceptT . flip runReaderT cfg . unMane

withDetachedKernelDriverIfCapable :: USB.Ctx -> DeviceHandle -> Interface -> IO a -> IO a
withDetachedKernelDriverIfCapable ctx devHndl i m
  | USB.hasCapability ctx USB.SupportsDetachKernelDriver = withDetachedKernelDriver devHndl i m
  | otherwise = m

withFTDI 
  :: ManeConfig
  -> (InterfaceHandle -> Mane a)
  -> IO (Either ManeFailure a)
withFTDI cfg m = do
  findFPGADevice cfg >>= \case
    Left failure -> return $ Left failure
    Right (usbDevice, ctx) -> do
      ftdiDevice <- fromUSBDevice usbDevice ChipType_2232H
      withDeviceHandle ftdiDevice $ \devHndl -> do
#ifdef mingw32_HOST_OS
        let devHndl' = setTimeout devHndl 10000000
#else
        let devHndl' = devHndl
#endif
        resetUSB devHndl'
        withDetachedKernelDriverIfCapable ctx devHndl' Interface_A $
          withInterfaceHandle devHndl' Interface_A $ \ifHndl -> do
            reset ifHndl
            purgeReadBuffer ifHndl
            purgeWriteBuffer ifHndl
            setLatencyTimer ifHndl 1
            setBitMode ifHndl 0xFF BitMode_MPSSE
            runMane cfg $ m ifHndl

-- | Program SPI Flash
progFlash :: ManeConfig -> FilePath -> IO (Either ManeFailure ())
progFlash cfg file = withFTDI cfg $ \ifHndl -> do
  liftIO $ putStrLn "init..."
  printCDone ifHndl
  flashReleaseReset ifHndl
  liftIO $ threadDelay 100000
  liftIO $ putStrLn "reset..."
  flashChipDeselect ifHndl
  liftIO $ threadDelay 250000
  printCDone ifHndl
  flashReset ifHndl
  flashPowerUp ifHndl
  flashReadID ifHndl
  flashWriteEnable ifHndl
  flashChipErase ifHndl
  flashWait ifHndl

  -- program flash
  liftIO $ putStrLn "programming..."
  progResult <- liftIO $ withFile file ReadMode $ \fileHndl -> 
    runMane cfg $ chunkProg ifHndl fileHndl 0
  case progResult of
    Left failure -> throwError failure
    Right () -> return ()

  flashPowerDown ifHndl
  flashReleaseReset ifHndl
  liftIO $ threadDelay 250000
  printCDone ifHndl
  liftIO $ putStrLn "Bye."

chunkProg :: InterfaceHandle -> Handle -> Integer -> Mane ()
chunkProg ifHndl fileHndl addr = do
  bytes <- liftIO $ BS.hGet fileHndl 256
  if BS.null bytes
    then return ()
    else do
      flashWriteEnable ifHndl
      flashProg addr bytes ifHndl
      flashWait ifHndl
      chunkProg ifHndl fileHndl $ addr + fromIntegral (BS.length bytes)

-- | Print the JEDEC ID of the connected FTDI device.
printJedecID :: ManeConfig -> IO (Either ManeFailure ())
printJedecID cfg = withFTDI cfg $ \ifHndl -> do
-- initialize USB to FT2232H
  liftIO $ putStrLn "init..."
  flashReleaseReset ifHndl
  liftIO $ threadDelay 100000
  flashChipDeselect ifHndl
  liftIO $ threadDelay 250000
  flashReset ifHndl
  flashPowerUp ifHndl
  flashReadID ifHndl
  flashReleaseReset ifHndl
  liftIO $ threadDelay 100000
  liftIO $ putStrLn "Bye."

-- | Toggle CRESET
toggleReset :: ManeConfig -> IO (Either ManeFailure ())
toggleReset cfg = withFTDI cfg $ \ifHndl -> do
  liftIO $ putStrLn "toggle reset..."
  flashReleaseReset ifHndl
  liftIO $ threadDelay 40000
  flashChipDeselect ifHndl
  liftIO $ threadDelay 100000
  flashReleaseReset ifHndl
  liftIO $ putStrLn "Bye."

spiXfer :: InterfaceHandle -> ByteString -> Mane BS.ByteString
spiXfer ifHndl cmd = do
  flashChipSelect ifHndl
  recv <- runFTDI ifHndl $ readWriteBytes Falling MsbFirst cmd
  flashChipDeselect ifHndl
  return recv

spiWrite :: InterfaceHandle -> BS.ByteString -> Mane ()
spiWrite ifHndl cmd = do
  flashChipSelect ifHndl
  runFTDI ifHndl $ writeBytes Falling MsbFirst cmd
  flashChipDeselect ifHndl

flashReset :: InterfaceHandle -> Mane ()
flashReset ifHndl = spiWrite ifHndl $ BS.pack $ replicate 8 0xFF

flashReadID :: InterfaceHandle -> Mane ()
flashReadID ifHndl = do
  flashID <- spiXfer ifHndl $ BS.pack $ 0x9F : replicate 3 0
  liftIO $ putStrLn $ "flash ID: " ++ showBS (BS.tail flashID)

flashReadStatus :: InterfaceHandle -> Mane Word8
flashReadStatus ifHndl = fmap BS.last $ spiXfer ifHndl $ BS.pack [0x05, 0x00]

flashWriteEnable :: InterfaceHandle -> Mane ()
flashWriteEnable ifHndl = spiWrite ifHndl $ BS.singleton 0x06

flashChipErase :: InterfaceHandle -> Mane ()
flashChipErase ifHndl = spiWrite ifHndl $ BS.singleton 0x60

{-
flashWriteStatusEnable :: InterfaceHandle -> Mane ()
flashWriteStatusEnable ifHndl = spiWrite ifHndl $ BS.singleton 0x50
-}
flashPowerUp :: InterfaceHandle -> Mane ()
flashPowerUp ifHndl = spiWrite ifHndl $ BS.singleton 0xAB

flashPowerDown :: InterfaceHandle -> Mane ()
flashPowerDown ifHndl = spiWrite ifHndl $ BS.singleton 0xB9

flashProg :: Integer -> ByteString -> InterfaceHandle -> Mane ()
flashProg addr bytes ifHndl = spiWrite ifHndl $ BS.cons 0x02 $ toFlashAddr addr `BS.append` bytes

{-
flashRead :: Integer -> InterfaceHandle -> Mane Word8
flashRead addr ifHndl = fmap BS.last $ spiXfer ifHndl $ BS.cons 0x03 $ toFlashAddr addr `BS.snoc` 0x00
-}
flashWait :: InterfaceHandle -> Mane ()
flashWait ifHndl = do
  status <- flashReadStatus ifHndl
  when (status .&. 0x01 == 1) $ do
    liftIO $ threadDelay 1000
    flashWait ifHndl

getCDone :: InterfaceHandle -> Mane Bool
getCDone ifHndl = do
  bankByte <- BS.head <$> runFTDI ifHndl (getGpioValue BankL)
  return $ (0x40 .&. bankByte) /= 0

printCDone :: InterfaceHandle -> Mane ()
printCDone ifHndl = getCDone ifHndl >>= \cdone -> 
  liftIO $ putStrLn $ if cdone
    then "cdone: high"
    else "cdone: low"

flashReleaseReset :: InterfaceHandle -> Mane ()
flashReleaseReset ifHndl = runFTDI ifHndl $ setCsCreset True True

flashChipSelect :: InterfaceHandle -> Mane ()
flashChipSelect ifHndl = runFTDI ifHndl $ setCsCreset False False

flashChipDeselect :: InterfaceHandle -> Mane ()
flashChipDeselect ifHndl = runFTDI ifHndl $ setCsCreset True False

runFTDI :: InterfaceHandle -> Command a -> Mane a
runFTDI ifHndl cmd = liftIO (run ifHndl cmd) >>= \case
  Left failure -> throwError $ FTDIFailure failure
  Right a      -> return a

setCsCreset :: Bool -> Bool -> Command ()
setCsCreset cs creset = setGpioDirValue BankL $ allInputs{ gpio0 = Output True
                                                         , gpio1 = Output True
                                                         , gpio4 = Output cs
                                                         , gpio7 = Output creset
                                                         }

toFlashAddr :: Integer -> ByteString
toFlashAddr a = let addr2 = fromInteger $ shiftR a 16
                    addr1 = fromInteger $ shiftR a 8
                    addr0 = fromInteger a
                 in BS.pack [addr2, addr1, addr0]

showBS :: BS.ByteString -> String
showBS = foldr (\n rest -> showHex n . showChar ' ' $ rest) "" . BS.unpack

-- | Find the first USB device matching the vendor ID and product ID
findFPGADevice :: ManeConfig -> IO (Either ManeFailure (USB.Device, USB.Ctx))
findFPGADevice cfg = do
  ctx <- USB.newCtx
  devDescs <- getDeviceDescs ctx
  return $ case fst <$> find (match . snd) devDescs of
    Nothing  -> Left FPGANotFound
    Just dev -> Right (dev, ctx)
  where
    match :: USB.DeviceDesc -> Bool
    match devDesc =  USB.deviceVendorId  devDesc == vendorId cfg
                  && USB.deviceProductId devDesc == productId cfg

getDeviceDescs :: USB.Ctx -> IO [(USB.Device, USB.DeviceDesc)]
getDeviceDescs ctx = do
  devs <- V.toList <$> USB.getDevices ctx
  deviceDescs <- mapM USB.getDeviceDesc devs
  return $ zip devs deviceDescs

-- | Get device information from a USB device.
deviceInfo :: USB.Device -> [String]
deviceInfo dev =
  [ printf "deviceSpeed:   %s" (maybe "-" show $ USB.deviceSpeed dev)
  , printf "busNumber:     %s" (show $ USB.busNumber dev)
  , printf "portNumber:    %s" (show $ USB.portNumber dev)
  , printf "portNumbers:   %s" (maybe "-" (show . V.toList) $
                                  USB.portNumbers dev 7)
  , printf "deviceAddress: %s" (show $ USB.deviceAddress dev)
  ]

