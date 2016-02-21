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
import Util
import Url

friends :: [String]
friends = ["asi1024"] -- please input your friends

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
                 Just dat -> filter (\x -> userScreenName x `elem` friends) dat
                 Nothing  -> error "Contest doesn't exist"
      html $ renderHtml $ $(hamletFile "./template/standings.hamlet") undefined
