import Prelude hiding (readFile)
import Control.Applicative ((<$>))
import Control.Exception (catch, SomeException)
import Control.Monad (mapM, liftM2)
import Data.Char (isDigit)
import Data.List (isPrefixOf, sortBy)
import Data.Function (on)
import System.Directory (getDirectoryContents)
import System.IO.Strict (readFile)
import Text.Printf (printf)

import Control.Function (mapSnd)
import Math.Number (filesize)
import Text.String (trChar)

type Pid = String

format = "%5s %9s %s"
totalFmt = "Total: %8s"

main = do
  d <- mapM swapusedWithPid =<< pids
  let printResult r = do
        putStrLn $ printf format "PID" "SWAP" "COMMAND"
        putStr . unlines $ r
        putStrLn $ printf totalFmt $ filesize $ (* 1024) $ total d
  printResult =<< mapM formatResult (transformData d)
    where swapused' p = swapused p `catch` handler
          handler :: SomeException -> IO Int
          handler e = return 0
          swapusedWithPid p = liftM2 (,) (return p) $ swapused' p

pids :: IO [Pid]
pids = filter digitsOnly <$> getDirectoryContents "/proc"
  where digitsOnly = all (`elem` ['0'..'9'])

swapused :: Pid -> IO Int
swapused pid = sum . map getNumber . filter (isPrefixOf "Swap:") . lines <$> readFile ("/proc/" ++ pid ++ "/smaps")
  where getNumber = read . takeWhile isDigit . dropWhile (not.isDigit)

transformData :: [(Pid, Int)] -> [(Pid, String)]
transformData = map (mapSnd humanSize) . sortBy (compare `on` snd) . filter ((/=) 0 . snd)
  where humanSize = filesize . (* 1024)

formatResult :: (Pid, String) -> IO String
formatResult (pid, size) = do
  cmd <- getCommand pid
  return $ printf format pid size cmd

getCommand :: Pid -> IO String
getCommand pid = trChar '\0' ' ' <$> readFile ("/proc/" ++ pid ++ "/cmdline")

total :: [(Pid, Int)] -> Int
total = sum . map snd
