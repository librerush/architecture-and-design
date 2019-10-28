{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Model
  ( Student(..)
  , Staff(..)
  , Course(..)
  ) where

import           GHC.Generics               (Generic)

import           Data.Text                  (Text)
import           Data.Vector                (Vector)
import           Database.PostgreSQL.Simple

data Student = Student
  { idStudent      :: Int
  , nameStudent    :: Text
  , scoreStudent   :: Integer
  , coursesStudent :: Vector Text
  } deriving (Show, Generic, FromRow)

data Course = Course
  { idCourse          :: Int
  , nameCourse        :: Text
  , descriptionCourse :: Text
  , materialsCourse   :: Vector Text
  } deriving (Show, Generic, FromRow)

data Staff = Staff
  { idStaff      :: Int
  , nameStaff    :: Text
  , staffCourses :: Vector Text
  } deriving (Show, Generic, FromRow)
