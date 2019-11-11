module Main where

import           Data.Text (Text)
import qualified System.Console.GetOpt as Console
import qualified System.Environment (getArgs)

import           Twerk (Tweet)
import qualified Twerk as Twerk
import qualified Twerk.Pipes as Twerk

import Numeric.Natural

data TopOption
   = ReplyToUsers
   | RetweetOfUsers
   | HandlesInTweet
   | Hashtags
   deriving (Eq, Read, Show)

data CountOption
   = Replies
   | RT

data TwerkOpts
   = Top TopOption
   | Count CountOption
   deriving (Eq, Read, Show)

topOpts :: [String]

cliArgs :: [String] -> (TwerkOpts, FilePath)
cliArgs (mode:opts) = case mode of
  "top" -> topOpts opts

cliArgs [] = fail "Usage: twerk [top|count] ARCHIVE.csv"

main :: IO ()
main = undefined
