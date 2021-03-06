module ChaosBox
  ( runChaosBoxWith
  , runChaosBoxIO
  , runChaosBoxIOWith
  , fillScreenHSV
  , Opts(..)
  , module ChaosBox.Generate
  , module ChaosBox.Color
  ) where

import           ChaosBox.Color
import           ChaosBox.Generate

import           Control.Monad.Random
import           Control.Monad.Reader
import           Data.IORef
import           Data.Maybe
import           Data.Semigroup           ((<>))
import           Data.Time.Clock.POSIX
import           Graphics.Rendering.Cairo
import           Options.Applicative
import           System.Directory

data Opts = Opts
  { optSeed           :: Maybe Int
  , optScale          :: Double
  , optWidth          :: Int
  , optHeight         :: Int
  , optRenderTimes    :: Int
  , optName           :: String
  , optRenderProgress :: Bool
  , optMetadataString :: Maybe String
  }

opts :: Parser Opts
opts =
  Opts
    <$> optional (option auto $ long "seed" <> metavar "SEED")
    <*> option auto (long "scale" <> metavar "SCALE" <> value 1)
    <*> option auto (long "width" <> short 'w' <> metavar "WIDTH" <> value 100)
    <*> option auto
               (long "height" <> short 'h' <> metavar "HEIGHT" <> value 100)
    <*> option auto (long "times" <> metavar "TIMES" <> value 1)
    <*> strOption (long "name" <> metavar "NAME" <> value "sketch")
    <*> switch (long "render-progress" <> short 'r')
    <*> optional (strOption (long "metadata" <> metavar "METADATA"))

optsInfo :: ParserInfo Opts
optsInfo = info
  (opts <**> helper)
  (fullDesc <> progDesc "Generate art with ChaosBox" <> header "chaosbox")

runChaosBoxIO
  :: RandT StdGen (ReaderT GenerateCtx Render) a
  -- ^ Render function
  -> IO ()
runChaosBoxIO render = do
  options <- execParser optsInfo
  runChaosBoxWith options render

runChaosBoxIOWith
  :: (Opts -> Opts)
  -- ^ Option modifier
  -> RandT StdGen (ReaderT GenerateCtx Render) a
  -- ^ Render function
  -> IO ()
runChaosBoxIOWith fn render = do
  options <- execParser optsInfo
  runChaosBoxWith (fn options) render

runChaosBoxWith
  :: Opts
  -- ^ Art options
  -> RandT StdGen (ReaderT GenerateCtx Render) a
  -- ^ Render function
  -> IO ()
runChaosBoxWith Opts {..} doRender = replicateM_ optRenderTimes $ do
  seed <- case optSeed of
    Just seed' -> pure seed'
    Nothing    -> round . (* 1000) <$> getPOSIXTime

  let stdGen = mkStdGen seed
      w      = round $ fromIntegral optWidth * optScale
      h      = round $ fromIntegral optHeight * optScale

  surface     <- createImageSurface FormatARGB32 w h
  progressRef <- newIORef 0

  -- Create directories if they don't exist
  createDirectoryIfMissing False $ "./images/"
  createDirectoryIfMissing False $ "./images/" <> optName
  createDirectoryIfMissing False $ "./images/" <> optName <> "/progress"

  beforeSaveHookRef <- newIORef Nothing

  let ctx = GenerateCtx optWidth
                        optHeight
                        seed
                        optScale
                        optName
                        optRenderProgress
                        progressRef
                        beforeSaveHookRef

  void . renderWith surface . flip runReaderT ctx . flip runRandT stdGen $ do
    cairo $ scale optScale optScale
    void doRender

    ref   <- asks gcBeforeSaveHook
    mHook <- liftIO $ readIORef ref

    case mHook of
      Nothing   -> pure ()
      Just hook -> hook

  putStrLn "Generating art..."
  let filename =
        "images/"
          <> optName
          <> "/"
          <> show seed
          <> "-"
          <> show optScale
          <> fromMaybe "" optMetadataString
          <> ".png"
      latest = "images/" <> optName <> "/latest.png"

  putStrLn $ "Writing " <> filename
  putStrLn $ "Writing " <> latest

  surfaceWriteToPNG surface filename
  surfaceWriteToPNG surface latest

-- Utility function

fillScreenHSV :: HSV -> Generate ()
fillScreenHSV color = do
  (w, h) <- getSize
  cairo $ do
    rectangle 0 0 w h
    setSourceHSV color *> fill
