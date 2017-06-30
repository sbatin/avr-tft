{-# LANGUAGE RecordWildCards #-}

module Main where

import Codec.BMP
import Control.Monad
import Data.Bits
import Data.Char
import Data.List
import Data.Word
import Font
import System.FilePath
import System.IO
import Text.Printf
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

type FontData = BS.ByteString

stringify :: FontData -> String
stringify = intercalate "," . map (printf "0x%02X") . BS.unpack

renderSymbol :: Char -> FontData -> IO ()
renderSymbol ch font = do
    putStrLn line
    forM_ [0..pred h] $ \y -> do
        putChar '|'
        forM_ [0..pred w] $ \x -> do
            let pos = x + w * y
                (j, i) = pos `divMod` 8
                byte = BSU.unsafeIndex font (s + j)

            putChar $ if testBit byte (7 - i) then '#' else ' '
        putChar '|'
        putChar '\n'

    putStrLn line
    where
        [x_size, y_size, offset, numchars] = BS.unpack $ BS.take 4 font
        w = fromIntegral x_size
        h = fromIntegral y_size
        n = h * w `div` 8
        s = 4 + (ord ch - fromIntegral offset) * n

        line = "+" ++ replicate w '-' ++ "+"

type RGBA = ((Int, Int), BS.ByteString)
type Pixel = (Word8, Word8, Word8)
type Cell = [[Bool]]

loadData :: FilePath -> IO RGBA
loadData filePath = do
    Right bmp <- readBMP filePath
    return (bmpDimensions bmp, unpackBMPToRGBA32 bmp)

getPixel :: (Int, Int) -> RGBA -> Pixel
getPixel (x, y) ((w, h), rgba) = (r, g, b) where
    start = (x + (pred h - y) * w) * 4

    [r, g, b] = take 3 $ map (BSU.unsafeIndex rgba) [start..]

hasData :: Pixel -> Bool
hasData (r, g, b) = r < 255 || g < 255 || b < 255

takeCellData :: Int -> (Int, Int) -> RGBA -> Cell
takeCellData cellIndex cellSize rgba = map (\y -> map (\x -> hasData $ getPixel (x, y) rgba) [x1..x2]) [y1..y2] where
    headerHeight = 17

    cellsInRow = fst (fst rgba) `div` (fst cellSize - 1)

    (cellY, cellX) = cellIndex `divMod` cellsInRow

    x1 = cellX * (fst cellSize - 1)
    y1 = headerHeight + 1 + cellY * (headerHeight + snd cellSize)
    x2 = x1 + fst cellSize - 1
    y2 = y1 + snd cellSize - 1

packFontData :: Cell -> FontData
packFontData = BS.pack . loop . concat where
    loop [] = []
    loop xs = byte : loop zs where
        byte = foldl' (\a (b, i) -> if b then setBit a (7 - i) else a) 0 $ zip ys [0..]
        (ys, zs) = splitAt 8 xs

drawCellData :: Cell -> String
drawCellData cell = unlines (line : loop cell ++ [line]) where
    line = "+" ++ map (const '-') (head cell) ++ "+"

    loop [] = []
    loop (xs:xss) = row : loop xss where
        row = '|' : map (\b -> if b then '#' else ' ') xs ++ "|"

data Options = Options {
    filePath :: FilePath,
    cellSize :: (Int, Int),
    fontSize :: (Int, Int),
    fontOffset :: (Int, Int)
}

data Font = Font {
    fontData :: FontData,
    fontNumChars :: Int
}

loadFont :: Options -> IO Font
loadFont Options{..} = do
    rgba <- loadData filePath

    let
        start = ' '
        chars = [start..'Z']
        header = BS.pack $ map fromIntegral [fst fontSize, snd fontSize, ord start, length chars]
        charsData = map (\ch -> toFontData $ takeCellData (ord ch - ord start) cellSize rgba) chars

    return Font {
        fontData = BS.append header $ BS.concat charsData,
        fontNumChars = length chars
    }
    where
        slice n m = take n . drop m

        toFontData = packFontData . slice (snd fontSize) (snd fontOffset) . map (slice (fst fontSize) (fst fontOffset))

saveFont :: String -> Font -> IO ()
saveFont fontName Font{..} = do
    h <- openFile ("ili9481/" ++ fontName ++ ".c") WriteMode
    hPutStrLn h "#include <avr/pgmspace.h>"
    hPutStrLn h $ printf "const uint8_t %s[%d] PROGMEM={" fontName (BS.length fontData)
    hPutStrLn h $ stringify header ++ ","

    let loop n xs = unless (BS.null xs) $ do
            let (content, xs') = BS.splitAt charDataSize xs
            hPutStrLn h $ stringify content ++ ", // " ++ [chr n]
            loop (succ n) xs'

    loop (fromIntegral $ BSU.unsafeIndex header 2) charsData
    hPutStrLn h "};"
    hClose h
    where
        (header, charsData) = BS.splitAt 4 fontData
        charDataSize = BS.length charsData `div` fontNumChars

--renderSymbol2 :: Char -> RGBA -> IO ()
--renderSymbol2 ch rgba = putStrLn $ drawCellData $ takeFontData $ takeCellData (ord ch - 32) rgba

font1 = Options {
    filePath = "/Users/sergey/Desktop/test-2.bmp",
    cellSize = (34, 35),
    fontSize = (16, 24),
    fontOffset = (8, 5)
}

font2 = Options {
    filePath = "/Users/sergey/Desktop/test.bmp",
    cellSize = (42, 43),
    fontSize = (20, 28),
    fontOffset = (10, 7)
}

main = do
    font@Font{..} <- loadFont font1
    --renderSymbol2 '0' rgba
    --renderSymbol2 '4' rgba
    --renderSymbol 'W' fontdatatype

    saveFont "SmallCDU" font
    renderSymbol '0' fontData
    renderSymbol '4' fontData
    renderSymbol 'V' fontData
