
module DB
  ( initDbConnection
  , addStudent
  , addStaff
  , getStudentByName
  , getStaffByName
  , isStudentAccount
  , isStaffAccount
  ) where

import           Model

import           Control.Exception          (SomeException, try)
import           Data.Int                   (Int64)
import           Data.Monoid                ((<>))
import           System.Environment

import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BC
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple


getDbEnv :: IO (Maybe ByteString)
getDbEnv = fmap BC.pack <$> lookupEnv "PSQL_STR"

-- | Connect to db
initDbConnection :: IO (Maybe Connection)
initDbConnection = do
  envMaybe <- getDbEnv
  case envMaybe of
    Just env -> do
      tryConn <- try (connectPostgreSQL env)
        :: IO (Either SomeException Connection)
      case tryConn of
        Right conn -> pure $ Just conn
        Left  _    -> pure Nothing
    Nothing  -> pure Nothing

addStudent :: Connection -> Text -> Text -> IO Int64
addStudent !conn !sname !spasswd = do
  execute conn "INSERT INTO student (name, password, score) VALUES \
    \(?, ?, ?)" (sname, spasswd, 0 :: Int)

-- | Get student by name (first occurence)
getStudentByName :: Connection -> Text -> IO (Maybe Student)
getStudentByName !conn !name = do
  let !q = "SELECT id, name, score, courses FROM student WHERE name = ?"
  !res <- query conn q (Only name) :: IO [Student]
  case res of
    [student] -> pure $ Just student
    _         -> pure $ Nothing

-- | Validation
isStudentAccount :: Connection -> (Text, Text) -> IO Bool
isStudentAccount conn acc = do
  let !sq = "SELECT id FROM student WHERE name = ? AND password = ?"
  !res <- query conn sq acc :: IO [Only Int]
  case res of
    [_] -> pure True
    _   -> pure False

isStaffAccount :: Connection -> (Text, Text) -> IO Bool
isStaffAccount conn acc = do
  let !sq = "SELECT id FROM staff WHERE name = ? AND password = ?"
  !res <- query conn sq acc :: IO [Only Int]
  case res of
    [_] -> pure True
    _   -> pure False

addStaff :: Connection -> Text -> Text -> IO Int64
addStaff !conn !sname !spasswd = do
  execute conn "INSERT INTO staff (name, password) VALUES \
    \(?, ?)" (sname, spasswd)

-- | Get staff by name (first occurence)
getStaffByName :: Connection -> Text -> IO (Maybe Staff)
getStaffByName !conn !name = do
  let !q = "SELECT id, name, courses FROM staff WHERE name = ?"
  !res <- query conn q (Only name) :: IO [Staff]
  case res of
    [staff] -> pure $ Just staff
    _       -> pure $ Nothing


