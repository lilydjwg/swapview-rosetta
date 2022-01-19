{-# LANGUAGE OverloadedStrings, Strict, TypeApplications #-}
import Control.Applicative ((<$>))
import Control.Exception (catch, SomeException)
import Control.Monad (mapM, liftM2)
import qualified Control.Monad.Parallel as MP
import Data.Char (isDigit)
import Data.List (sortBy)
import Data.Function (on)
import System.Directory (getDirectoryContents)
import Text.Printf (printf)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Either (fromRight)

type Pid = Int

format = "%7d %9s %s"
formatHead = "%7s %9s %s"
totalFmt = "Total: %10s"
firstLine = printf formatHead ("PID" :: String) ("SWAP" :: String) ("COMMAND" :: String)

main = do
  d <- MP.mapM swapusedWithPid =<< pids
  let printResult r = do
        putStrLn firstLine
        BS.putStr . BS.unlines $ r
        putStrLn $ printf totalFmt $ BS.unpack . filesize $ (* 1024) $ total d
  printResult =<< mapM formatResult (transformData d)
    where swapused' p = swapused p `catch` handler
          handler :: SomeException -> IO Int
          handler e = return 0
          swapusedWithPid p = liftM2 (,) (return p) $ swapused' p

pids :: IO [Pid]
pids = map read . filter (all isDigit) <$> getDirectoryContents "/proc"

swapused :: Pid -> IO Int
swapused pid = sum . map getNumber . filter (BS.isPrefixOf "Swap:") . BS.lines <$> BS.readFile ("/proc/" ++ show pid ++ "/smaps")
  where getNumber = (read @Int) . BS.unpack . BS.takeWhile isDigit . BS.dropWhile (not . isDigit)

transformData :: [(Pid, Int)] -> [(Pid, ByteString)]
transformData = map (mapSnd humanSize) . sortBy (compare `on` snd) . filter ((/=) 0 . snd)
  where humanSize = filesize . (* 1024)

formatResult :: (Pid, ByteString) -> IO ByteString
formatResult (pid, size) = do
  cmd <- getCommand pid
  return . BS.pack $ printf format pid (BS.unpack size) (BS.unpack cmd)

getCommand :: Pid -> IO ByteString
getCommand pid = BS.map transnul . dropLastNull <$> BS.readFile ("/proc/" ++ show pid ++ "/cmdline")
  where dropLastNull s
          | BS.null s = s
          | BS.last s == '\0' = BS.init s
          | otherwise = s
        transnul ch = if ch == '\0' then ' ' else ch

total :: [(Pid, Int)] -> Int
total = sum . map snd

units = "KMGTP"

liftUnit :: Double -> String -> Char -> (Double, Char)
liftUnit n u l =
  if n > 1100 && not (null u)
     then liftUnit (n / 1024) (tail u) (head u)
     else (n, l)

filesize :: (Integral a, Show a) => a -> ByteString
filesize n = BS.pack $
  if unit /= '\0'
     then printf "%.1f%ciB" m unit
     else show n ++ "B"
  where (m, unit) = liftUnit (fromIntegral n) units '\0'

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (a, b) = (a, f b)
