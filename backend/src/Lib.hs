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
import qualified Database.PostgreSQL.Simple           as Db (close)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Web.Scotty


main :: IO ()
main = do
  conn <- initDbConnection
  when (isNothing conn) $ error "initDbConnection failed"
  studentRef <- newIORef defStudent

  scotty 8080 $ do
    middleware logStdoutDev
    -- middleware $ staticPolicy (noDots >-> addBase "static")

    get "/" $
      html startingPage

    get "/signup" $ do
      html signUpPage

    get "/signin" $ do
      html signInPage

    get "/home" $ do
      student <- liftIO $ readIORef studentRef
      html $ homePage student

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
              idMaybe <- liftIO $ getStudentId (fromJust conn) name
              case idMaybe of
                Just id_ -> do
                  liftIO $ writeIORef studentRef
                    (defStudent {idStudent = id_, nameStudent = name})
                  redirect "/home"
                Nothing  -> redirect "/"
            ("staff" :: Text)   -> do
              void . liftIO $ addStaff (fromJust conn) name password
              redirect "/"
            _                   -> text "Select student or staff"

  void . pure $ Db.close <$> conn
