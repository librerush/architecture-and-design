{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  (main)
  where

import           DB
import           Model
import           View

import           Control.Monad                        (void, when)
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.Maybe                           (fromJust, isNothing)

import           Data.ByteString                      (ByteString)
import           Data.Text                            (Text)
import qualified Data.Text.Lazy                       as LT
import qualified Database.PostgreSQL.Simple           as Db (close)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Web.Scotty


main :: IO ()
main = do
  conn <- initDbConnection
  when (isNothing conn) $ error "initDbConnection failed"
  studentRef <- newIORef defStudent
  staffRef <- newIORef defStaff

  scotty 8080 $ do
    middleware logStdoutDev
    -- middleware $ staticPolicy (noDots >-> addBase "static")

    get "/" $
      html startingPage

    get "/avatar.png" $
      file "static/avatar.png"

    get "/signup" $ do
      html signUpPage

    get "/signin" $ do
      html signInPage

    get "/log-out" $ do
      liftIO $ writeIORef studentRef defStudent
      liftIO $ writeIORef staffRef defStaff
      redirect "/"

    post "/create/course" $ do
      html createCoursePage

    post "/create/new_course" $ do
      name <- param "course_name"
      desc <- param "course_desc"
      void . liftIO $ addCourse (fromJust conn) name desc
      redirect $ ("/add/lecture/" :: LT.Text) <> LT.fromStrict name

    get "/add/lecture/:course" $ do
      name <- param "course"
      html $ addLecturePage name

    post "/add/lecture/content/:name" $ do
      name <- param "name"
      content <- param "lec_text"
      let !cadd = encodeCourseAdd $ CourseAdd (LT.toStrict content) []
      void . liftIO $ addLecture (fromJust conn) name cadd
      text content

    get "/home-student" $ do
      student <- liftIO $ readIORef studentRef
      html $ homeStudentPage student

    get "/home-staff" $ do
      staff <- liftIO $ readIORef staffRef
      html $ homeStaffPage staff

    post "/signin-post" $ do
      name <- param "username"
      password <- param "password"
      isStudent <- liftIO $
        isStudentAccount (fromJust conn) (name, password)
      isStaff <- liftIO $
        isStaffAccount (fromJust conn) (name, password)
      if isStudent then do
        student_ <- liftIO $ getStudentByName (fromJust conn) name
        case student_ of
          Just student -> do
            liftIO $ writeIORef studentRef student
            redirect "/home-student"
          Nothing -> text $ "no student has such name"
      else if isStaff then do
        staff_ <- liftIO $ getStaffByName (fromJust conn) name
        case staff_ of
          Just staff -> do
            liftIO $ writeIORef staffRef staff
            redirect "/home-staff"
          Nothing -> text $ "no staff has such name"
      else
        text "there is no such user"


    post "/signup-post" $ do
      name <- param "username"
      password <- param "password"
      repeatpasswd <- param "repeatpasswd"
      who <- param "who"
      if password /= repeatpasswd then
        text "password and repeated password must be the same"
      else case who of
            ("student" :: Text) -> do
              void . liftIO $ addStudent (fromJust conn) name password
              maybeStudent_ <- liftIO $ getStudentByName (fromJust conn) name
              case maybeStudent_ of
                Just maybeStudent -> do
                  liftIO $ writeIORef studentRef maybeStudent
                  redirect "/home-student"
                Nothing -> redirect "/"
            ("staff" :: Text)   -> do
              void . liftIO $ addStaff (fromJust conn) name password
              staffMaybe_ <- liftIO $ getStaffByName (fromJust conn) name
              case staffMaybe_ of
                Just staffMaybe -> do
                  liftIO $ writeIORef staffRef staffMaybe
                  liftIO $ print staffMaybe
                  redirect "/home-staff"
                Nothing -> redirect "/"
            _                   -> text "Select student or staff"

  void . pure $ Db.close <$> conn
