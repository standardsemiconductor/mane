
import qualified System.USB as USB
import Options.Applicative
import Mane

data Mane = FindDevice Bool
          | ReadJedec Bool
          | ToggleReset Bool
          | Program FilePath

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
maneParser =     findDeviceParser
              <|> readJedecParser
              <|> toggleResetParser
              <|> programParser

main :: IO ()
main = mane =<< execParser opts
  where
    opts = info (maneParser <**> helper)
      ( fullDesc
     <> progDesc "program bitstream"
     <> header "mane - VELDT programmer - Standard Semiconductor" )

mane :: Mane -> IO ()
mane = \case
  FindDevice True  -> findFPGADevice >>= USB.getDeviceDesc.fst >>= print
  ReadJedec True   -> printJedecID
  ToggleReset True -> toggleReset
  Program file     -> progFlash file
  _                -> return ()

