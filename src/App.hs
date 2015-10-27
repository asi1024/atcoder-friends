module App where

getUrl :: String -> String
getUrl s = "https://" ++ s ++ ".contest.atcoder.jp/standings"

app :: IO()
app = do
  str <- getLine
  putStrLn $ getUrl str
