{-# LANGUAGE BangPatterns, OverloadedStrings, ScopedTypeVariables #-}

-- Thanks to przhu
-- https://gist.github.com/przhu/7892814

import Control.Applicative ((<$>))
import Control.Exception (catch, SomeException)
import Control.Monad (mapM)
import Data.Char (isDigit)
import Data.List (sortBy)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Format as T
import Data.Text.Lazy(toStrict)
import Data.Text.Read(decimal)
import System.Directory (getDirectoryContents)
import Text.Printf (printf)
import Control.Arrow(second)

filesize :: (Integral a, Show a) => a -> String
filesize n =
  if not.null $ level
     then printf "%.1f%ciB" m unit
     else show n ++ "B"
  where (m, level) = liftUnit (fromIntegral n) units []
        unit = head level

liftUnit :: Double -> [Char] -> [Char] -> (Double, [Char])
liftUnit n u l =
  if n > 1100 && (not.null) u
     then liftUnit (n/1024) (tail u) (head u :l)
     else (n, l)

units = "KMGTP"

type Pid = T.Text

pidTitl :: T.Text
pidTitl = "PID"
swapTitl :: T.Text
swapTitl = "SWAP"
cmdTitl :: T.Text
cmdTitl = "COMMAND"

format _1 _2 _3 = toStrict $ T.format "{} {} {}" (b1, b2, b3) where
  b1 = T.left 5 ' ' _1
  b2 = T.left 9 ' ' _2
  b3 = _3
totalFmt _1 = toStrict $ T.format "Total: {}" (T.Only b1) where
  b1 = T.left 8 ' ' _1

main = do
  ps <- pids
  ss <- mapM swapusedNoExcept ps
  let !t = 1024 * sum ss
  r <- mapM formatResult (transformData (zip ps ss))
  let printResult = do
        T.putStrLn $ format pidTitl swapTitl cmdTitl
        T.putStr . T.unlines $ r
        T.putStrLn $ totalFmt $ filesize t
  printResult
    where swapusedNoExcept !p = do
              su <- catch (swapused p) (\(_::SomeException) -> return 0)
              return $! su

pids :: IO [Pid]
pids = filter digitsOnly . map T.pack <$> getDirectoryContents "/proc"
  where digitsOnly = T.all isDigit

swapused :: Pid -> IO Int
swapused pid = sum . map getNumber . filter (T.isPrefixOf "Swap:") . T.lines <$>
                 T.readFile (T.unpack $ "/proc/" `T.append` pid `T.append` "/smaps")
  where getNumber line =
          case T.dropWhile (not.isDigit) line of
            t -> case decimal t of
                   (Right (n, _)) -> n
                   (Left _) -> 0

transformData :: [(Pid, Int)] -> [(Pid, String)]
transformData = map (second humanSize) .
                sortBy (\ (_, !x) (_, !y) -> compare x y) .
                filter ((/=) 0 . snd)
  where humanSize = filesize . (* 1024)

formatResult :: (Pid, String) -> IO T.Text
formatResult (pid, size) = do
  cmd <- getCommand pid
  return $ format pid size cmd

getCommand :: Pid -> IO T.Text
getCommand pid = T.init <$> T.map transnul <$> T.readFile (T.unpack $ "/proc/" `T.append` pid `T.append` "/cmdline")
  where transnul ch = if ch == '\0' then ' ' else ch

