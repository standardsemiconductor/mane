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
  ) where

import qualified System.USB as USB
import qualified Data.ByteString as BS
import qualified Data.Vector as V (toList)
import Data.ByteString (ByteString)
import Numeric (showHex)
import System.FTDI
import System.FTDI.MPSSE
import Data.Bits
import Data.Word
import Control.Monad
import Control.Concurrent (threadDelay)
import Text.Printf (printf)
import Data.List (find)
import System.Exit (exitFailure)
import System.IO

withDetachedKernelDriverIfCapable :: USB.Ctx -> DeviceHandle -> Interface -> IO a -> IO a
withDetachedKernelDriverIfCapable ctx devHndl i m
  | USB.hasCapability ctx USB.SupportsDetachKernelDriver = withDetachedKernelDriver devHndl i m
  | otherwise = m

withFTDI :: (InterfaceHandle -> IO a) -> IO a
withFTDI m = do
  (usbDevice, ctx) <- findFPGADevice
  ftdiDevice <- fromUSBDevice usbDevice ChipType_2232H
  withDeviceHandle ftdiDevice $ \devHndl -> do
    resetUSB devHndl
    withDetachedKernelDriverIfCapable ctx devHndl Interface_A $
      withInterfaceHandle devHndl Interface_A $ \ifHndl -> do
        reset ifHndl
        purgeReadBuffer ifHndl
        purgeWriteBuffer ifHndl
        setLatencyTimer ifHndl 1
        setBitMode ifHndl 0xFF BitMode_MPSSE
        run ifHndl enableClkDivBy5 >>= \case
          Left failure -> error $ show failure
          Right _      -> return ()
        run ifHndl (setClockDivisor 0) >>= \case
          Left failure -> error $ show failure
          Right _      -> return ()
        m ifHndl

progFlash :: FilePath -> IO ()
progFlash file = withFTDI $ \ifHndl ->
  withFile file ReadMode $ \fileHndl -> do
    putStrLn "init..."
    printCDone ifHndl
    flashReleaseReset ifHndl
    threadDelay 100000
    putStrLn "reset..."
    flashChipDeselect ifHndl
    threadDelay 250000
    printCDone ifHndl
    flashReset ifHndl
    flashPowerUp ifHndl
    flashReadID ifHndl
    flashWriteEnable ifHndl
    flashChipErase ifHndl
    flashWait ifHndl
    putStrLn "programming..."
    chunkProg ifHndl fileHndl 0
    flashPowerDown ifHndl
    flashReleaseReset ifHndl
    threadDelay 250000
    printCDone ifHndl
    putStrLn "Bye."

chunkProg :: InterfaceHandle -> Handle -> Integer -> IO ()
chunkProg ifHndl fileHndl addr = do
  bytes <- BS.hGet fileHndl 256
  if BS.null bytes
    then return ()
    else do
      flashWriteEnable ifHndl
      flashProg addr bytes ifHndl
      flashWait ifHndl
      chunkProg ifHndl fileHndl $ addr + fromIntegral (BS.length bytes)

printJedecID :: IO ()
printJedecID = withFTDI $ \ifHndl -> do
-- initialize USB to FT2232H
  putStrLn "init..."
  flashReleaseReset ifHndl
  threadDelay 100000
  flashChipDeselect ifHndl
  threadDelay 250000
  flashReset ifHndl
  flashPowerUp ifHndl
  flashReadID ifHndl
  flashReleaseReset ifHndl
  threadDelay 100000
  putStrLn "Bye."

toggleReset :: IO ()
toggleReset = withFTDI $ \ifHndl -> do
  putStrLn "toggle reset..."
  flashReleaseReset ifHndl
  threadDelay 40000
  flashChipDeselect ifHndl
  threadDelay 100000
  flashReleaseReset ifHndl
  putStrLn "Bye."

spiXfer :: InterfaceHandle -> ByteString -> IO BS.ByteString
spiXfer ifHndl cmd = do
  flashChipSelect ifHndl
  recv <- run ifHndl (readWriteBytes Falling MsbFirst cmd) >>= \case
    Left failure -> error $ show failure
    Right recv   -> return recv
  flashChipDeselect ifHndl
  return recv

spiWrite :: InterfaceHandle -> BS.ByteString -> IO ()
spiWrite ifHndl cmd = do
  flashChipSelect ifHndl
  run ifHndl (writeBytes Falling MsbFirst cmd) >>= \case
    Left failure -> error $ show failure
    Right _      -> return ()
  flashChipDeselect ifHndl

flashReset :: InterfaceHandle -> IO ()
flashReset ifHndl = spiWrite ifHndl $ BS.pack $ replicate 8 0xFF

flashReadID :: InterfaceHandle -> IO ()
flashReadID ifHndl = do
  flashID <- spiXfer ifHndl $ BS.pack $ 0x9F : replicate 3 0
  putStrLn $ "flash ID: " ++ showBS (BS.tail flashID)

flashReadStatus :: InterfaceHandle -> IO Word8
flashReadStatus ifHndl = fmap BS.last $ spiXfer ifHndl $ BS.pack [0x05, 0x00]

flashWriteEnable :: InterfaceHandle -> IO ()
flashWriteEnable ifHndl = spiWrite ifHndl $ BS.singleton 0x06

flashChipErase :: InterfaceHandle -> IO ()
flashChipErase ifHndl = spiWrite ifHndl $ BS.singleton 0x60

flashWriteStatusEnable :: InterfaceHandle -> IO ()
flashWriteStatusEnable ifHndl = spiWrite ifHndl $ BS.singleton 0x50

flashPowerUp :: InterfaceHandle -> IO ()
flashPowerUp ifHndl = spiWrite ifHndl $ BS.singleton 0xAB

flashPowerDown :: InterfaceHandle -> IO ()
flashPowerDown ifHndl = spiWrite ifHndl $ BS.singleton 0xB9

flashProg :: Integer -> ByteString -> InterfaceHandle -> IO ()
flashProg addr bytes ifHndl = spiWrite ifHndl $ BS.cons 0x02 $ toFlashAddr addr `BS.append` bytes

flashRead :: Integer -> InterfaceHandle -> IO Word8
flashRead addr ifHndl = fmap BS.last $ spiXfer ifHndl $ BS.cons 0x03 $ toFlashAddr addr `BS.snoc` 0x00

flashWait :: InterfaceHandle -> IO ()
flashWait ifHndl = do
  status <- flashReadStatus ifHndl
  when (status .&. 0x01 == 1) $ do
    threadDelay 1000
    flashWait ifHndl

getCDone :: InterfaceHandle -> IO Bool
getCDone ifHndl = run ifHndl (readB BankL) >>= \case
  Left failure -> error $ show failure
  Right a -> return $ (0x40 .&. BS.head a) /= 0

printCDone :: InterfaceHandle -> IO ()
printCDone ifHndl = getCDone ifHndl >>= \cdone -> if cdone
  then putStrLn "cdone: high"
  else putStrLn "cdone: low"

flashReleaseReset :: InterfaceHandle -> IO ()
flashReleaseReset ifHndl = run ifHndl (setCsCreset True True) >>= \case
  Left failure -> error $ show failure
  Right _      -> return ()

flashChipSelect :: InterfaceHandle -> IO ()
flashChipSelect ifHndl = run ifHndl (setCsCreset False False) >>= \case
  Left failure -> error $ show failure
  Right _      -> return ()

flashChipDeselect :: InterfaceHandle -> IO ()
flashChipDeselect ifHndl = run ifHndl (setCsCreset True False) >>= \case
  Left failure -> error $ show failure
  Right _      -> return ()

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

findFPGADevice :: IO (USB.Device, USB.Ctx)
findFPGADevice = do
  ctx <- USB.newCtx
  devs <- V.toList <$> USB.getDevices ctx
  deviceDescs <- mapM USB.getDeviceDesc devs
  case fmap fst $ find (match . snd) $ zip devs deviceDescs of
    Nothing  -> hPutStrLn stderr "FPGA not found" >> exitFailure
    Just dev -> return (dev, ctx)
  where
    match :: USB.DeviceDesc -> Bool
    match devDesc =  USB.deviceVendorId  devDesc == vendorId
                  && USB.deviceProductId devDesc == productId

vendorId :: USB.VendorId
vendorId = 0x403

productId :: USB.ProductId
productId = 0x6014

deviceInfo :: USB.Device -> [String]
deviceInfo dev =
  [ printf "deviceSpeed:   %s" (maybe "-" show $ USB.deviceSpeed dev)
  , printf "busNumber:     %s" (show $ USB.busNumber dev)
  , printf "portNumber:    %s" (show $ USB.portNumber dev)
  , printf "portNumbers:   %s" (maybe "-" (show . V.toList) $
                                  USB.portNumbers dev 7)
  , printf "deviceAddress: %s" (show $ USB.deviceAddress dev)
  ]

