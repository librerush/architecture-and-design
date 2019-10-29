{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Model
  ( Student (..)
  , Staff (..)
  , Course (..)
  , defStudent
  , defCourse
  , defStaff
  ) where

import           GHC.Generics               (Generic)

import           Data.Text                  (Text)
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import           Database.PostgreSQL.Simple

data Student = Student
  { idStudent      :: !Int
  , nameStudent    :: !Text
  , scoreStudent   :: !Int
  , coursesStudent :: !(Vector Text)
  } deriving (Show, Generic, FromRow)

data Course = Course
  { idCourse          :: !Int
  , nameCourse        :: !Text
  , descriptionCourse :: !Text
  , materialsCourse   :: !(Vector Text)
  } deriving (Show, Generic, FromRow)

data Staff = Staff
  { idStaff      :: !Int
  , nameStaff    :: !Text
  , coursesStaff :: !(Vector Text)
  } deriving (Show, Generic, FromRow)


defStudent :: Student
defStudent = Student
  { idStudent = 0
  , nameStudent = ""
  , scoreStudent = 0
  , coursesStudent = V.empty
  }

defCourse :: Course
defCourse = Course
  { idCourse = 0
  , nameCourse = ""
  , descriptionCourse = ""
  , materialsCourse = V.empty
  }

defStaff :: Staff
defStaff = Staff
  { idStaff = 0
  , nameStaff = ""
  , coursesStaff = V.empty
  }
