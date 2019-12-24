{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Model
  ( Student (..)
  , Staff (..)
  , Course (..)
  , defStudent
  , defCourse
  , defStaff
  , encodeCourseAdd
  , decodeCourseAdd
  , CourseQuest(..)
  , CourseAdd(..)
  ) where

import           GHC.Generics               (Generic)

import           Data.Aeson                 (FromJSON, ToJSON, decode', encode)
import           Data.Text                  (Text)
import qualified Data.Text.Lazy             as TL
import           Data.Text.Lazy.Encoding    (decodeUtf8, encodeUtf8)
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import           Database.PostgreSQL.Simple


data Student = Student
  { idStudent      :: !Int
  , nameStudent    :: !Text
  , scoreStudent   :: !Int
  , coursesStudent :: !(Vector Int)
  } deriving (Eq, Show, Generic, FromRow)


data Course = Course
  { idCourse          :: !Int
  , nameCourse        :: !Text
  , descriptionCourse :: !Text
  , materialsCourse   :: !Text
  } deriving (Eq, Show, Generic, FromRow)


data Staff = Staff
  { idStaff      :: !Int
  , nameStaff    :: !Text
  , coursesStaff :: !(Vector Int)
  } deriving (Eq, Show, Generic, FromRow)


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
  , materialsCourse = ""
  }


defStaff :: Staff
defStaff = Staff
  { idStaff = 0
  , nameStaff = ""
  , coursesStaff = V.empty
  }


data CourseQuest = CourseQuest
  { cQuestion :: !Text
  , cAnswer   :: !Text
  , cVariants :: ![Text]
  } deriving (Generic, Eq, Show, ToJSON, FromJSON)


data CourseAdd = CourseAdd
  { caddLec  :: !Text
  , caddQuiz :: ![CourseQuest]
  } deriving (Generic, Eq, Show, ToJSON, FromJSON)


encodeCourseAdd :: CourseAdd -> Text
encodeCourseAdd = TL.toStrict . decodeUtf8 . encode


decodeCourseAdd :: Text -> Maybe CourseAdd
decodeCourseAdd = decode' . encodeUtf8 . TL.fromStrict
