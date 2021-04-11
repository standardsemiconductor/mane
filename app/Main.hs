
import qualified System.USB as USB
import Options.Applicative
import Mane
import Numeric ( showHex )

data Mane = FindDevice Bool
          | ReadJedec Bool
          | ToggleReset Bool
          | Program FilePath

data Opts = Opts
  { optMane    :: Mane
  , optVendor  :: USB.ProductId
  , optProduct :: USB.ProductId
  }

programParser :: Parser Mane
programParser = Program <$> strArgument (metavar "FILE")

findDeviceParser :: Parser Mane
findDeviceParser = FindDevice
  <$> switch
      ( long "find"
     <> short 'f'
     <> help "Find FPGA device, print description if found." )

readJedecParser :: Parser Mane
readJedecParser = ReadJedec
  <$> switch
      ( long "jedec"
     <> short 'j'
     <> help "Read JEDEC ID" )

toggleResetParser :: Parser Mane
toggleResetParser  = ToggleReset
  <$> switch
      ( long "reset"
     <> short 'r'
     <> help "Toggle CRESETB" )

maneParser :: Parser Mane
maneParser =
      findDeviceParser
  <|> readJedecParser
  <|> toggleResetParser
  <|> programParser                    

vendorParser :: Parser USB.VendorId
vendorParser =
  option auto
         ( long "vendor"
        <> value 0x403
        <> metavar "VENDOR_ID"
        <> showDefaultWith (\v -> "0x" ++ showHex v "")
        <> help "Use the specified USB device vendor ID.")      

productParser :: Parser USB.ProductId
productParser =
  option auto
         ( long "product"
        <> value 0x6014
        <> metavar "PRODUCT_ID"
        <> showDefaultWith (\p -> "0x" ++ showHex p "")
        <> help "Use the specified USB device product ID.")

optsParser :: Parser Opts
optsParser = Opts <$> maneParser <*> vendorParser <*> productParser

main :: IO ()
main = mane =<< execParser opts
  where
    opts = info (optsParser <**> helper)
      ( fullDesc
     <> progDesc "program bitstream"
     <> header "mane - VELDT programmer - Standard Semiconductor" )

mane :: Opts -> IO ()
mane opts = case optMane opts of
  FindDevice True  -> putStrLn . unlines . deviceInfo . fst =<< findFPGADevice cfg
  ReadJedec True   -> printJedecID cfg
  ToggleReset True -> toggleReset cfg
  Program file     -> progFlash cfg file
  _ -> return ()
  where
    cfg = ManeConfig
      { vendorId  = optVendor opts
      , productId = optProduct opts
      }


