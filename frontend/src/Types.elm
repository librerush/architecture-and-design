module Types exposing
  ( Model (..)
  , Msg (..)
  , Student
  , Staff
  , Course
  , studentEncode
  , studentDecode
  , staffEncode
  , staffDecode
  , courseEncode
  , courseDecode
  )

import Json.Encode as E
import Json.Decode as D

type Model
  = StartingPage
  | SignUpPage
  | UserPage
  | CoursePage

type Msg
  = GotSignUpPage
  | GotStartPage

type alias Student =
  { id      : Int
  , name    : String
  , score   : Int
  , courses : List String
  }

type alias Staff =
  { id      : Int
  , name    : String
  , courses : List String 
  }

type alias Course =
  { id          : Int
  , name        : String
  , description : String
  , materials   : List String
  }

studentEncode : Student -> E.Value
studentEncode student =
  E.object
    [ ("id",      E.int student.id)
    , ("name",    E.string student.name)
    , ("score",   E.int student.score)
    , ("courses", E.list E.string student.courses)
    ]

studentDecode : D.Decoder Student
studentDecode =
  D.map4 Student
    (D.field "id"      D.int)
    (D.field "name"    D.string)
    (D.field "score"   D.int)
    (D.field "courses" (D.list D.string))

staffEncode : Staff -> E.Value
staffEncode staff =
  E.object
    [ ("id",      E.int staff.id)
    , ("name",    E.string staff.name)
    , ("courses", E.list E.string staff.courses)
    ]

staffDecode : D.Decoder Staff
staffDecode =
  D.map3 Staff
    (D.field "id"      D.int)
    (D.field "name"    D.string)
    (D.field "courses" (D.list D.string))

courseEncode : Course -> E.Value
courseEncode course =
  E.object
    [ ("id",          E.int course.id)
    , ("name",        E.string course.name)
    , ("description", E.string course.description)
    , ("materials",   E.list E.string course.materials)
    ]

courseDecode : D.Decoder Course
courseDecode =
  D.map4 Course
    (D.field "id"          D.int)
    (D.field "name"        D.string)
    (D.field "description" D.string)
    (D.field "materials"   (D.list D.string))


