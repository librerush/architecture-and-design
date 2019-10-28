
module Lib
  (main)
  where

import           DB
import           View

import           Control.Monad                        (void, when)
import           Control.Monad.IO.Class
import           Data.Maybe                           (isNothing)

import           Data.ByteString                      (ByteString)
import qualified Database.PostgreSQL.Simple           as Db (close)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.Wai.Middleware.Static        (addBase, noDots,
                                                       staticPolicy, (>->))
import           Web.Scotty


main :: IO ()
main = do
  conn <- initDbConnection
  when (isNothing conn) $ error "initDbConnection failed"

  scotty 8080 $ do
    middleware logStdoutDev
    -- middleware $ staticPolicy (noDots >-> addBase "static")

    get "/" $
      html startingPage

    get "/signup" $ do
      html signUpPage

    get "/signin" $ do
      html signInPage


  void . pure $ Db.close <$> conn
