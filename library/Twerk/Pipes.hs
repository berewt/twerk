{- | 'Twerk.Pipes' provides 'Pipes' machinery to load tweets through a
  'Producer' and process them.

  Here are a few examples of pipes that can be build:

  @
  extractHashtagsFromRTs :: Pipe (Either a Tweet) Hashtag r
  extractHashtagsFromRTs = filterRight >-> filter (isJust . retweet) >-> mapFoldable (hashtags . content)

  topUsersReply :: Natural -> Pipe Tweet [(UserId, Natural)] r
  topUsersreply n = mapFoldable reply >-> map authorId >-> occurencesScan >-> top n
  @

  As this module re-exports 'Pipes.Prelude', the module is intended to be imported qualified.
-}
module Twerk.Pipes
  ( -- * Producer
    produceTweets
    -- * Pipes
    -- ** Filtering parsing results
  , filterLeft
  , filterRight
  , containing
  , containingInsensitive
  , between
    -- ** Scaning results
  , occurencesScan
  , top
    -- * Folds
  , occurencesFold
    -- * Misc
  , ite
    -- * Re-exports
  , module P
  , module IO
  ) where

import           Control.Applicative       (liftA2)
import           Control.Monad.IO.Class    (MonadIO)
import qualified Data.List              as List
import           Data.Map                  (Map)
import qualified Data.Map               as Map
import qualified Data.Ord               as Ord
import           Data.Text                 (Text)
import qualified Data.Text              as Text
import           Numeric.Natural           (Natural)
import           Pipes                     (Pipe, Producer)
import           Pipes                  as P
import           Pipes.Prelude          as P
import qualified Pipes.ByteString       as PB
import qualified Pipes.Csv              as CSV
import           System.IO              as IO (Handle, withFile, IOMode (..))

import           Twerk

-- | Given the handle of a twitter archive, produce tweets or parsing errors.
--
-- Example (print all the parsing results):
--
-- @
-- WithFile "tweets.csv" ReadMode (\ h -> runEffect $ produceTweets h >-> P.print)
-- @
produceTweets :: MonadIO m => Handle -> Producer (Either String Tweet) m ()
produceTweets h = CSV.decode CSV.HasHeader (PB.fromHandle h)


-- | Filter left values from 'Either'.
filterLeft :: Monad m => Pipe (Either a b) a m r
filterLeft = mapFoldable (either pure Left)

-- | Filter right values from 'Either'. An alias for @concat@.
filterRight :: Monad m => Pipe (Either a b) b m r
filterRight = P.concat

-- | Build a cumulative occurences 'Map' of the incoming data
occurencesScan :: (Ord a, Monad m) => Pipe a (Map a Natural) m r
occurencesScan = let
  go = pure . maybe 1 (+1)
  in scan (flip $ Map.alter go) Map.empty id

-- | Fold the produced value as an occurence map.
--
-- Cumulative map might not be interesting witha raw archive
-- as tweets are organised in an antichronological order.
--
-- If you intend to use all the results, you may want to
-- reverse the content of the file first.
occurencesFold :: (Ord a, Monad m) => Producer a m () -> m (Map a Natural)
occurencesFold = let
  go = pure . maybe 1 (+1)
  in fold (flip $ Map.alter go) Map.empty id

-- | Get the nth key-value pairs of a Map
top :: (Ord b, Num b, Monad m) => Natural -> Pipe (Map a b) [(a, b)] m r
top n = let
  compareOccurences = Ord.comparing $ Ord.Down . snd
  in P.map $ List.genericTake n . List.sortBy compareOccurences . Map.toList

-- | Filter texts that contain a given substring
containing :: Monad m => Text -> Pipe Tweet Tweet m r
containing q = P.filter $ List.any (q `Text.isPrefixOf`) . Text.tails . content

-- | Filter texts that contain a given substring (case insensitive)
containingInsensitive :: Monad m => Text -> Pipe Tweet Tweet m r
containingInsensitive q = let
  predicate = Text.isPrefixOf . Text.toLower
  in P.filter $ List.any (predicate q) . Text.tails . Text.toLower . content

-- | Check if a value is between a lower bound and a higher bound (if provided)
between :: (Ord b, Monad m) => Maybe b -> Maybe b -> (a -> b) -> Pipe a a m r
between lo hi = let
  go = maybe (const True)
  predicate = liftA2 (&&) (go (<=) lo) (go (>=) hi)
  in P.filter . (predicate .)

-- | If-then-else
ite :: Monad m => (a -> Bool) -> Pipe a b m () -> Pipe a c m () -> Pipe a (Either b c) m ()
ite predicate l r = for cat $ \x -> yield x >-> if predicate x
  then l >-> P.map Left
  else r >-> P.map Right
