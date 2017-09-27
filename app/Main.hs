module Main where

import Control.Monad
import Data.List
--import Lib
--import Network.Wreq (get)
import System.Environment

--getWiki :: String -> IO (Response ByteString)
--getWiki topic = get ("https://en.wikipedia.org/wiki/" ++ topic)

main :: IO ()
main = do
  (start, end) <- getStops
  return ()

getStops :: IO (String, String)
getStops = do
  args <- getArgs
  case args of (start:end:_) -> return (start, end)
               _ -> error "stop"
