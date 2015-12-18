module Show where

import Graphics.Gloss

showBmp :: [String] -> Int -> IO ()
showBmp [file] _ = do
  pic <- loadBMP file
  display (InWindow "gmfrm" (16,16) (100,100)) black pic
showBmp files _ = do
  pics <- mapM loadBMP files
  let a f = pics !! (fromInteger (round (f * 5)) `mod` (length pics))
  animate (InWindow "gmfrm" (16,16) (100,100)) black a
