import Control.Monad
import Control.Monad.Primitive
import Codec.Picture
import Codec.Picture.Types
import Options.Applicative

import Problem


data Options
  = Options
  { optProblemNo :: Int
  , optOutput :: FilePath
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> problemNo
  <*> fileOutput
  where
    problemNo = argument auto
      $  metavar "PROBLEM_NO"
      <> help "problem number"

    fileOutput :: Parser (FilePath)
    fileOutput = strOption
      $  short 'o'
      <> metavar "FILE"
      <> help "output image filename"
      <> showDefaultWith id


parserInfo :: ParserInfo Options
parserInfo = info (optionsParser <**> helper)
  $  fullDesc
  <> header "draw-problem"


main :: IO ()
main = do
  opt <- execParser parserInfo

  Just prob <- readProblem (optProblemNo opt)
  img <- createMutableImage (ceiling (room_width prob) + 1) (ceiling (room_height prob) + 1) (PixelRGB8 255 255 255)

  let (x0, y0) = stage_bottom_left prob
  -- stage
  forM_ [floor x0 .. ceiling (x0 + stage_width prob)] $ \x ->
    forM_ [floor y0 .. ceiling (y0 + stage_height prob)] $ \y ->
      writePixelBT img x y (PixelRGB8 0 0 0)

  -- attendees
  forM_ (attendees prob) $ \Attendee{ x = x, y = y } -> do
    forM_ [max 0 (round x - 5) .. min (ceiling (room_width prob)) (round x + 5)] $ \x' -> do
      forM_ [max 0 (round y - 5) .. min (ceiling (room_height prob)) (round y + 5)] $ \y' -> do
        writePixelBT img x' y' (PixelRGB8 0 255 255)
  forM_ (attendees prob) $ \Attendee{ x = x, y = y } -> do
    writePixelBT img (round x) (round y) (PixelRGB8 0 0 255)

  writePng (optOutput opt) =<< freezeImage img


writePixelBT :: (Pixel a, PrimMonad m) => MutableImage (PrimState m) a -> Int -> Int -> a -> m ()
writePixelBT img x y px = writePixel img x (mutableImageHeight img - 1 - y) px
