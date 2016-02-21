module Fetch where

import Data.ByteString.Lazy.Char8 (unpack)
import Data.ByteString.Lazy.Internal (ByteString)
import Data.List (isPrefixOf)

import Network.HTTP.Conduit
import Network

import Url

fetch :: String -> IO ByteString
fetch cid = withSocketsDo $ do
  request' <- parseUrl $ standingsUrl cid
  let request = request' { checkStatus = \_ _ _ -> Nothing }
  manager <- newManager tlsManagerSettings
  res <- httpLbs request manager
  return $ responseBody res

getJsonStr :: ByteString -> String
getJsonStr str =
  reverse $ remove "]}" $ reverse $ remove "[{" $ concat.words $ unpack str
  where remove s = until (\x -> null x || isPrefixOf s x) tail
