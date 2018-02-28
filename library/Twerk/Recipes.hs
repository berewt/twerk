module Twerk.Recipes
  ( module Twerk.Recipes
  ) where

import Control.Lens (view)
import Data.Function (on)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Ord as O
import qualified Data.Time as T
import qualified Data.Time.Calendar.WeekDate as T
import Numeric.Natural

import           Pipes ((>->))
import qualified Pipes.Prelude as P
import qualified Pipes.Group as PG

import Twerk
import Twerk.Pipes


validTweets :: MonadIO m => Handle -> Producer Tweet m ()
validTweets h = produceTweets h >-> filterRight

runOnFile :: (Handle -> IO a) -> FilePath -> IO a
runOnFile go f = withFile f ReadMode go

timeProcessing :: (Handle -> T.TimeZone -> IO a) -> FilePath -> IO a
timeProcessing go f = do
  tz <- T.getCurrentTimeZone
  withFile f ReadMode (`go` tz)


infoFromValidTweets :: MonadIO m => (Tweet -> a) -> Handle -> Producer a m ()
infoFromValidTweets f h = validTweets h >-> P.map f

tweetsHours :: MonadIO m => T.TimeZone -> Handle -> Producer Int m ()
tweetsHours tz = let
  extractHour = T.todHour . T.localTimeOfDay . T.utcToLocalTime tz . getTime . date
  in infoFromValidTweets extractHour

tweetsWeekOfDay :: MonadIO m => T.TimeZone -> Handle -> Producer Int m ()
tweetsWeekOfDay tz = let
  extractWeekOfDay =
    ( (\(_, _, d) -> d)
    . T.toWeekDate
    . T.localDay
    . T.utcToLocalTime tz
    . getTime
    . date
    )
 in infoFromValidTweets extractWeekOfDay

tweetsYears :: MonadIO m => Handle -> Producer Integer m ()
tweetsYears = let
  extractYear = ((\(y, _, _) -> y) . T.toGregorian . T.utctDay . getTime . date)
  in infoFromValidTweets extractYear

twweetsWithWord :: MonadIO m => Text -> Handle -> Producer Tweet m ()
twweetsWithWord word h = validTweets h >-> containingInsensitive word

mostFrequent :: Ord b => M.Map a b -> [(a,b)]
mostFrequent = L.sortOn (O.Down . snd) . M.toList

mostMentionedHandles :: Monad m => Producer Tweet m () -> m [(Text, Natural)]
mostMentionedHandles p =
  let extractHandle = p >-> P.mapFoldable (fmap getHandle . handles . content)
  in  mostFrequent <$> occurencesFold extractHandle

mostMentionedHashtags :: Monad m => Producer Tweet m () -> m [(Text, Natural)]
mostMentionedHashtags p =
  let extractHashtags =
        p >-> P.mapFoldable (fmap getHandle . handles . content)
  in  mostFrequent <$> occurencesFold extractHashtags

type YearMonth = (Integer, Int)

tweetMonth :: T.UTCTime -> YearMonth
tweetMonth = (\(y, m, _) -> (y, m)) . T.toGregorian . T.utctDay

mostMentionedHandlesByTimeInterval
  :: (Eq a, Monad m)
  => (T.UTCTime -> a)
  -> Producer Tweet m ()
  -> Producer (a, [(Text, Natural)]) m ()
mostMentionedHandlesByTimeInterval f p =
  let fFromTweet = f . getTime  . date
      tweetHandlesAsText = fmap getHandle . handles . content
      addHandle = flip $ M.alter (Just . maybe 1 (+ 1))
      addTweet Nothing tw = addTweet (Just (fFromTweet tw, M.empty)) tw
      addTweet (Just (k, acc)) tw =
        Just . (,) k . L.foldl' addHandle acc $ tweetHandlesAsText tw
      countSplitByMonth =
        PG.folds addTweet Nothing (fmap (fmap mostFrequent)) . view (PG.groupsBy ((==) `on` fFromTweet))
     in countSplitByMonth p >-> P.concat
