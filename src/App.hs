module App where

import Data.Maybe (fromJust)

import Fetch

getUrl :: String -> String
getUrl s = "https://" ++ s ++ ".contest.atcoder.jp/standings"

app :: IO()
app = do
  str <- getLine
  res <- fetch $ getUrl str
  putStrLn $ fromJust $ getJsonStr res
