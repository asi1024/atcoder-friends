module App where

import Data.Maybe (fromJust)

import Fetch
import Parse

friends :: [String]
friends = []

showPerson :: Person -> String
showPerson p = show (rank p) ++ "\t" ++ userName p ++ "\t" ++ score p

showFriends :: [Person] -> String
showFriends ps = unlines $ map showPerson $ filter isFriend ps
  where isFriend p = userName p `elem` friends

app :: IO()
app = do
  str <- getLine
  res <- fetch str
  let jsonstr = fromJust $ getJsonStr res
  putStrLn jsonstr
  let out = fromJust $ parseJson jsonstr
  putStrLn $ showFriends out
