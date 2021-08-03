{-# LANGUAGE OverloadedStrings, Strict #-}
import Control.Applicative ((<$>))
import Control.Exception (catch, SomeException)
import Control.Monad (mapM, liftM2)
import Data.Char (isDigit)
import Data.List (sortBy)
import Data.Function (on)
import System.Directory (getDirectoryContents)
import Text.Printf (printf)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text, isPrefixOf)
import Data.Text.Read (decimal)
import Data.Either (fromRight)

type Pid = Int

format = "%7d %9s %s"
formatHead = "%7s %9s %s"
totalFmt = "Total: %10s"
firstLine = printf formatHead ("PID" :: Text) ("SWAP" :: Text) ("COMMAND" :: Text)

main = do
  d <- mapM swapusedWithPid =<< pids
  let printResult r = do
        putStrLn firstLine
        TIO.putStr . T.unlines $ r
        putStrLn $ printf totalFmt $ filesize $ (* 1024) $ total d
  printResult =<< mapM formatResult (transformData d)
    where swapused' p = swapused p `catch` handler
          handler :: SomeException -> IO Int
          handler e = return 0
          swapusedWithPid p = liftM2 (,) (return p) $ swapused' p

pids :: IO [Pid]
pids = map read . filter (all isDigit) <$> getDirectoryContents "/proc"

swapused :: Pid -> IO Int
swapused pid = sum . map getNumber . filter (isPrefixOf "Swap:") . T.lines <$> TIO.readFile ("/proc/" ++ show pid ++ "/smaps")
  where getNumber = fst . fromRight undefined . decimal . T.dropWhile (not . isDigit)

transformData :: [(Pid, Int)] -> [(Pid, Text)]
transformData = map (mapSnd humanSize) . sortBy (compare `on` snd) . filter ((/=) 0 . snd)
  where humanSize = filesize . (* 1024)

formatResult :: (Pid, Text) -> IO Text
formatResult (pid, size) = do
  cmd <- getCommand pid
  return . T.pack $ printf format pid size cmd

getCommand :: Pid -> IO Text
getCommand pid = T.map transnul . dropLastNull <$> TIO.readFile ("/proc/" ++ show pid ++ "/cmdline")
  where dropLastNull s
          | T.null s = s
          | T.last s == '\0' = T.init s
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

filesize :: (Integral a, Show a) => a -> Text
filesize n = T.pack $
  if unit /= '\0'
     then printf "%.1f%ciB" m unit
     else show n ++ "B"
  where (m, unit) = liftUnit (fromIntegral n) units '\0'

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (a, b) = (a, f b)
