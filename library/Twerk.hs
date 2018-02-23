{-# LANGUAGE OverloadedStrings #-}

{- | Twerk (TWitter ERKhive reader) is a library for twitter archive parsing.

  The twitter archive is available from your twitter account:

  1. Click on your profile
  2. Go to _Settings and privacy_
  3. Select _Your Twitter data_
  4. At the bottom of the page, in the _Other Data_ section, click _Twitter Archive_

You'll receive your twitter archive on the email account associated to this twitter account.

In this archive, Twerk can process is the @twitter.csv@ file.

-}
module Twerk
  ( -- * Datatypes
    TweetId (..)
  , TweetTime (..)
  , EmbeddedURL (..)
  , TwitterClient (..)
  , TweetRef (..)
  , Tweet (..)
  , Hashtag (..)
  , TwitterHandle (..)
    -- * Extra accessors
  , hashtags
  , handles
    -- * Re-exports
  , module URL
  ) where

import           Control.Applicative (liftA2, liftA3)
import           Control.Monad ((<=<), guard, mfilter)
import qualified Data.ByteString as BS
import qualified Data.Char as Char
import           Data.Csv (FromField (..), ToField (..), FromRecord, ToRecord)
import qualified Data.Csv as CSV
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Time.Clock (UTCTime)
import qualified Data.Time.Format as Time
import           Network.URL (URL)
import qualified Network.URL as Network
import           Network.URL as URL (importURL, exportURL, URL (..))
import qualified Text.HTML.TagSoup as TagSoup

-- | A wrapper for internal Tweets identifier
newtype TweetId = TweetId { getTweetId :: Integer }
  deriving (Eq, Ord, Read, Show)

instance FromField TweetId where
  parseField = fmap TweetId . parseField

instance ToField TweetId where
  toField = toField . getTweetId

-- | A wrapper for internal Twitter's user numeric identifier (not their handle)
newtype UserId = UserId { getUserId :: Integer }
  deriving (Eq, Ord, Read, Show)

instance FromField UserId where
  parseField = fmap UserId . parseField

instance ToField UserId where
  toField = toField . getUserId

-- | Wrapper for UTCTime mainly used for Cassava parsing
newtype TweetTime = TweetTime { getTime :: UTCTime }
  deriving (Eq, Ord, Read, Show)

tweetTimeFormat :: String
tweetTimeFormat = "%F %T %z"

instance FromField TweetTime where
  parseField = fmap TweetTime
    . Time.parseTimeM True Time.defaultTimeLocale tweetTimeFormat
    . Text.unpack . Text.decodeUtf8

instance ToField TweetTime where
  toField = Text.encodeUtf8
    . Text.pack
    . Time.formatTime Time.defaultTimeLocale tweetTimeFormat
    . getTime

-- | Wrapped URL used to avoid orphan instances
newtype EmbeddedURL = EmbeddedURL { getURL :: URL }
  deriving (Eq, Show)

instance FromField EmbeddedURL where
  parseField = maybe (fail "Not a valid URL") (pure . EmbeddedURL) . Network.importURL <=< parseField

instance ToField EmbeddedURL where
  toField = Text.encodeUtf8 . Text.pack . Network.exportURL . getURL

-- | Client used to post the tweet
data TwitterClient = TwitterClient
  { name :: Text -- ^ client name
  , url :: URL  -- ^ client URL
  } deriving (Eq, Show)

instance FromField TwitterClient where
  parseField = let
    findHref tags = maybe (fail "Can't find device URI")
                          pure
                          $ do
      ot <- List.find (TagSoup.isTagOpenName "a") tags
      Network.importURL . Text.unpack . Text.decodeUtf8 $ TagSoup.fromAttrib "href" ot

    findName = maybe (fail "Unable to find device name")
                     (pure . (\(TagSoup.TagText s) -> Text.decodeUtf8 s))
                     . List.find TagSoup.isTagText
    go tags = liftA2 TwitterClient (findName tags) (findHref tags)
    in go . TagSoup.parseTags

instance ToField TwitterClient where
  toField = Text.encodeUtf8 . TagSoup.renderTags . sequence
    [ TagSoup.TagOpen "a" . (("rel", "nofollow"):) . pure . (,) "href" . Text.pack . Network.exportURL . url
    , TagSoup.TagText . name
    , const $ TagSoup.TagClose "a"
    ]

-- | Reference to a tweet (reply or RT)
data TweetRef = TweetRef
  { authorId :: UserId  -- ^ author of the initial tweet
  , statusId :: TweetId -- ^ identifier of the referenced tweet
  , refTime  :: Maybe TweetTime -- ^ timestamp of the referenced tweet
  } deriving (Eq, Show, Read)

-- | All the information relative to a tweet in your archive
data Tweet = Tweet
  { tweetId       :: !TweetId
  , reply         :: !(Maybe TweetRef)
  , retweet       :: !(Maybe TweetRef)
  , date          :: !TweetTime
  , twitterClient :: !TwitterClient
  , content          :: !Text
  , urls          :: ![Either Text EmbeddedURL]
  } deriving (Eq, Show)

instance FromRecord Tweet where
  parseRecord v | length v > 9 = let
    buildTweetRef = liftA3 (liftA3 TweetRef)
    go = liftA2 (>>) (guard . ("" /=))
                     (pure . fmap (List.drop 1) . List.break (',' ==))
    eitherImport x = maybe (Left $ Text.pack x)
                           (Right . EmbeddedURL)
                           (Network.importURL x)
    -- this unfold the list of expanded url provided in the archive as "url1,url2,..."
    urlList = fmap eitherImport . List.unfoldr go
    in Tweet <$> v CSV..! 0
             <*> buildTweetRef (v CSV..! 2) (v CSV..! 1) (pure $ pure Nothing)
             <*> buildTweetRef (v CSV..! 7) (v CSV..! 6) (v CSV..! 8)
             <*> v CSV..! 3
             <*> v CSV..! 4
             <*> v CSV..! 5
             <*> (urlList <$> v CSV..! 9)
                | otherwise = fail "Not enough fields to parse a tweet"

instance ToRecord Tweet where
  toRecord = CSV.record . sequence
    [ toField . tweetId
    , toField . fmap statusId . reply
    , toField . fmap authorId . reply
    , toField . date
    , toField . twitterClient
    , toField . content
    , toField . fmap statusId . retweet
    , toField . fmap authorId . retweet
    , toField . fmap refTime  . retweet
    , BS.intercalate "," . fmap (either toField toField) . urls
    ]

-- | Text Wrapper for twitter handles (the @name)
newtype TwitterHandle = TwitterHandle { getHandle :: Text }
  deriving (Eq, Ord, Read, Show)

-- | Text Wrapper for twitter hashtags (the #keywords)
newtype Hashtag = Hashtag { getHashtag :: Text }
  deriving (Eq, Ord, Read, Show)

isolateSpecificWords :: Text -> (Text -> a) -> Text -> [a]
isolateSpecificWords prefix constructor = let
  getHashtagName = mfilter (not . Text.null) . Just
                 . Text.takeWhile (liftA2 (||) ('_' ==) Char.isAlphaNum)
  startsWithHash = fmap snd . filter ((prefix ==) . fst) . fmap (Text.splitAt (Text.length prefix))
  in Foldable.toList . fmap constructor . getHashtagName <=< startsWithHash . Text.words


-- | Isolate hashtags in the content of a tweet.
-- The extracted hashtags doesn't contain the initial @#@
hashtags :: Text -> [Hashtag]
hashtags = isolateSpecificWords "#" Hashtag

-- | Isolate hashtags in the content of a tweet.
-- The extracted hashtags doesn't contain the initial @\@@
handles :: Text -> [TwitterHandle]
handles = isolateSpecificWords "@" TwitterHandle
