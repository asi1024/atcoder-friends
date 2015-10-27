module Fetch where

import Data.ByteString.Lazy.Char8 (unpack)
import Data.ByteString.Lazy.Internal (ByteString)

import Network.HTTP.Conduit
import Network

import Text.Regex

fetch :: String -> IO ByteString
fetch url = withSocketsDo $ do
  request' <- parseUrl url
  let request = request' { checkStatus = \_ _ _ -> Nothing }
  manager <- newManager tlsManagerSettings
  res <- httpLbs request manager
  return $ responseBody res

getJsonStr :: ByteString -> Maybe String
getJsonStr str = case matchRegexAll reg $ concat.words $ unpack str of
  Just (_, res, _, _) -> Just res
  Nothing -> Nothing
  where reg = mkRegex "ATCODER.standing.*};"
