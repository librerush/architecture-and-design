
module DB
  where

import           System.Environment

import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BC
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple

getEnvironment :: IO (Maybe ByteString)
getEnvironment = fmap BC.pack <$> lookupEnv "PSQL_STR"

