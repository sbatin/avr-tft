module Main where

import Codec.BMP
import Control.Monad
import Data.Bits
import Data.Char
import Data.List
import Data.Word
import Font
import System.FilePath
import Text.Printf
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

type Font = BS.ByteString

stringify :: Font -> String
stringify = intercalate "," . map (printf "0x%02X") . BS.unpack

renderSymbol :: Char -> Font -> IO ()
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

takeCellData :: Int -> RGBA -> Cell
takeCellData cellIndex rgba = map (\y -> map (\x -> hasData $ getPixel (x, y) rgba) [x1..x2]) [y1..y2] where
    cellSize = (42, 43)
    headerHeight = 17

    cellsInRow = fst (fst rgba) `div` (fst cellSize - 1)

    (cellY, cellX) = cellIndex `divMod` cellsInRow

    x1 = cellX * (fst cellSize - 1)
    y1 = headerHeight + 1 + cellY * (headerHeight + snd cellSize)
    x2 = x1 + fst cellSize - 1
    y2 = y1 + snd cellSize - 1

takeFontData :: Cell -> Cell
takeFontData = slice (snd fontSize) (snd fontOffset) . map (slice (fst fontSize) (fst fontOffset)) where
    slice n m = take n . drop m

    fontSize = (20, 28)
    fontOffset = (10, 7)

packFontData :: Cell -> Font
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

renderSymbol2 :: Char -> RGBA -> IO ()
renderSymbol2 ch rgba = putStrLn $ drawCellData $ takeFontData $ takeCellData (ord ch - 32) rgba

makeFontFile :: RGBA -> FilePath -> IO Font
makeFontFile rgba filePath = do
    let
        header = BS.pack [20, 28, 32, 95]
        symbols = map (\ch -> packFontData $ takeFontData $ takeCellData (ord ch - 32) rgba) [' '..'Z']
        size = BS.length header + sum (map BS.length symbols)

        content = printf "uint8_t Ubuntu[%d] PROGMEM={" size : map stringify (header:symbols) ++ ["};"]

    --putStrLn $ stringify (symbols !! 16)
    writeFile filePath $ intercalate ",\n" content
    return $ BS.append header $ BS.concat symbols

main = do
    rgba <- loadData "/Users/sergey/Desktop/test.bmp"
    renderSymbol2 '0' rgba
    renderSymbol2 '4' rgba
    renderSymbol 'W' fontdatatype

    font2 <- makeFontFile rgba "avr/CDU.c"
    renderSymbol '/' font2
