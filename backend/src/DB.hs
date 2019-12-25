
module DB
  ( initDbConnection
  , addCourse
  , addStudent
  , addStaff
  , getStudentByName
  , getStaffByName
  , getStudentId
  , isStudentAccount
  , isStaffAccount
  , getCourseAdd
  , addLecture
  , getCourseByName
  , getAllCoursesName
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
  execute conn "INSERT INTO student (name, password, score, courses) VALUES \
    \(?, ?, ?, ARRAY[]::integer[])" (sname, spasswd, 0 :: Int)


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
  execute conn "INSERT INTO staff (name, password, courses) VALUES \
    \(?, ?, ARRAY[]::integer[])" (sname, spasswd)


-- | Get staff by name (first occurence)
getStaffByName :: Connection -> Text -> IO (Maybe Staff)
getStaffByName !conn !name = do
  let !q = "SELECT id, name, courses FROM staff WHERE name = ?"
  !res <- query conn q (Only name) :: IO [Staff]
  case res of
    [staff] -> pure $ Just staff
    _       -> pure $ Nothing


getStudentId :: Connection -> Text -> IO (Maybe Int)
getStudentId !conn !name = do
  let !q = "SELECT id FROM student WHERE name = ?"
  !res <- query conn q (Only name) :: IO [Only Int]
  case res of
    [id_] -> pure $ Just $ fromOnly id_
    _     -> pure Nothing


addCourse :: Connection -> Text -> Text -> IO Int64
addCourse !conn !name !desc = do
  let !qs = "INSERT INTO course (name, description) VALUES \
    \(?, ?)"
  execute conn qs (name, desc)


getCourseAdd :: Connection -> Text -> IO (Maybe Text)
getCourseAdd !conn !name = do
  let !q = "SELECT materials FROM course WHERE name = ?"
  !res <- query conn q (Only name) :: IO [Only Text]
  case res of
    [cadd] -> pure $ Just $ fromOnly cadd
    _      -> pure Nothing


addLecture :: Connection -> Text -> Text -> IO Int64
addLecture !conn !name !content = do
  let !q = "UPDATE course SET materials = ? WHERE name = ?"
  execute conn q (content, name)


-- | Get course by name (first occurence)
getCourseByName :: Connection -> Text -> IO (Maybe Course)
getCourseByName !conn !name = do
  let !q = "SELECT * FROM course WHERE name = ?"
  !res <- query conn q (Only name) :: IO [Course]
  case res of
    [course] -> pure $ Just course
    _        -> pure $ Nothing


getAllCoursesName :: Connection -> IO [Text]
getAllCoursesName !conn = do
  let !q = "SELECT name FROM course"
  !res <- query_ conn q :: IO [Only Text]
  pure $ fromOnly  <$> res




