module Util where

import Data.Aeson

import qualified Data.HashMap.Strict as H
import qualified Data.Text as T

import Parse

mkProblemList :: [Person] -> [String]
mkProblemList ps = map (\x -> [fst x]) $ zip ['A'..]$ tasks $ head ps

showScore :: Person -> String
showScore p = show $ div (read $ score p :: Int) 100

showTime :: Int -> String
showTime t = show (div t 60) ++ ":"
          ++ show (mod (div t 10) 6) ++ show (mod t 10)

showPenalty :: Person -> String
showPenalty p = show (div t 60) ++ ":"
             ++ show (mod (div t 10) 6) ++ show (mod t 10)
  where t = read $ penalty p :: Int


taskScore :: Object -> Int
taskScore obj =
  case H.lookup (T.pack "score") obj of
    Just (Number x) -> floor $ x / 100
    Just _ ->  0
    Nothing -> 0

taskWrongAnswer :: Object -> Int
taskWrongAnswer obj =
  case H.lookup (T.pack "failure") obj of
    Just (Number x) -> floor x
    Just _ ->  0
    Nothing -> 0

taskElapsedTime :: Object -> Int
taskElapsedTime obj =
  case H.lookup (T.pack "elapsed_time") obj of
    Just (Number x) -> floor x
    Just _ ->  0
    Nothing -> 0

showTask :: Object -> (String, String, String)
showTask obj =
  case (taskScore obj, taskWrongAnswer obj, taskElapsedTime obj) of
    (0, 0, _) -> ("stUN", "-", "")
    (0, w, _) -> ("stWA", "(+" ++ show w ++ ")", "")
    (s, 0, t) -> ("stAC", show s, showTime t)
    (s, w, t) -> ("stAC", show s ++ " (+" ++ show w ++ ")", showTime t)
