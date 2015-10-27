module Parse where

import Control.Applicative (empty, (<$>), (<*>))

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BC8
import qualified Data.Text as T

data Task = Task
  { tTaskId      :: Int
  , tExtras      :: Bool
  , tScore       :: Int
  , tFailure     :: Int
  , tPenalty     :: Int
  , tElapsedTime :: Int
  } deriving (Eq, Show)

data Person = Person
  { rank           :: Int
  , contestId      :: Int
  , userId         :: Int
  , userName       :: String
  , userScreenName :: String
  , twitterId      :: String
  , score          :: String
  , elapsedTime    :: String
  , penalty        :: String
  , failure        :: String
  , tasks          :: [Object]
  } deriving (Eq, Show)

instance FromJSON Person where
  parseJSON (Object v) = Person
                     <$> v .: T.pack "rank"
                     <*> v .: T.pack "contest_id"
                     <*> v .: T.pack "user_id"
                     <*> v .: T.pack "user_name"
                     <*> v .: T.pack "user_screen_name"
                     <*> v .: T.pack "twitter_id"
                     <*> v .: T.pack "score"
                     <*> v .: T.pack "elapsed_time"
                     <*> v .: T.pack "penalty"
                     <*> v .: T.pack "failure"
                     <*> v .: T.pack "tasks"
  parseJSON _ = empty

parseJson :: String -> Maybe [Person]
parseJson str = decode $ BC8.pack str
