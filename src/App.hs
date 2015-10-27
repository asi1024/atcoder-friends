{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module App where

import Control.Monad.IO.Class (liftIO)

import Data.ByteString.Lazy.Internal (ByteString)

import Network.Wai.Middleware.RequestLogger

import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet

import Web.Scotty

import Fetch
import Parse
import Url

friends :: [String]
friends = []

showPerson :: Person -> String
showPerson p = show (rank p) ++ "\t" ++ userName p ++ "\t" ++ score p

showFriends :: [Person] -> String
showFriends ps = unlines $ map showPerson $ filter isFriend ps
  where isFriend p = userName p `elem` friends

app :: IO()
app = do
  scotty 36384 $ do
    middleware logStdoutDev

    get "/" $ do
      html $ renderHtml $ $(hamletFile "./template/index.hamlet") undefined

    get "/:cid" $ do
      cid <- param "cid" :: ActionM String
      str <- liftIO (fetch cid) :: ActionM ByteString
      let st = case getJsonStr str >>= parseJson of
                 Just dat -> filter (\x -> userName x `elem` friends) dat
                 Nothing  -> []
      html $ renderHtml $ $(hamletFile "./template/standings.hamlet") undefined
