module Url where

import Parse

endpoint :: String -> String
endpoint cid = "https://" ++ cid ++ ".contest.atcoder.jp/"

standingsUrl :: String -> String
standingsUrl cid = endpoint cid ++ "standings"

userPage :: String -> Person -> String
userPage cid p = endpoint cid ++ "users/" ++ userScreenName p
