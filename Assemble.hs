module Assemble where

import Codec.Picture

import System.IO
import Data.List (foldl')
import Data.ByteString (append)

assemble :: [String] -> String -> IO ()
assemble files o = do bmps <- mapM processFile files
                      writeBitmap o (concatBMP (concat bmps))

processFile :: String -> IO [Image PixelRGB8]
processFile file =
  do r <- readBitmap file
     case r of
       Left e ->
         do hPutStrLn stderr $ "Something went wrong when processing " ++ file
            hPutStrLn stderr e
            return []
       Right (ImageRGB8 bmp) | imageWidth bmp == 16 && imageHeight bmp == 16
                               -> return [bmp]
       Right _  -> do hPutStrLn stderr $ "Wrong format in " ++ file
                      return []

concatBMP :: [Image PixelRGB8] -> Image PixelRGB8
concatBMP images = generateImage gen 16 (16 * length images)
  where gen x y = pixelAt (images !! (y `quot` 16)) x (y `rem` 16)
