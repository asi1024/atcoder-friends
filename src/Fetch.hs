module Fetch where

import Data.ByteString.Lazy.Char8 (unpack)
import Data.ByteString.Lazy.Internal (ByteString)

import Network.HTTP.Conduit
import Network

import Text.Regex (mkRegex, matchRegexAll)

getUrl :: String -> String
getUrl s = "https://" ++ s ++ ".contest.atcoder.jp/standings"

fetch :: String -> IO ByteString
fetch cid = withSocketsDo $ do
  request' <- parseUrl $ getUrl cid
  let request = request' { checkStatus = \_ _ _ -> Nothing }
  manager <- newManager tlsManagerSettings
  res <- httpLbs request manager
  return $ responseBody res

getJsonStr :: ByteString -> Maybe String
getJsonStr str = case matchRegexAll reg $ concat.words $ unpack str of
  Just (_, res, _, _) -> Just res
  Nothing -> Nothing
  where reg = mkRegex "\\[\\{.*\\}\\]"
