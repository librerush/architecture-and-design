
module Lib
  (main)
  where

import           View

import           Control.Monad.IO.Class
import           System.Environment

import           Data.ByteString                      (ByteString)
import qualified Data.ByteString.Char8                as BC
import           Database.PostgreSQL.Simple
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.Wai.Middleware.Static        (addBase, noDots,
                                                       staticPolicy, (>->))
import           Web.Scotty

getEnvironment :: IO (Maybe ByteString)
getEnvironment = fmap BC.pack <$> lookupEnv "PSQL_STR"



main :: IO ()
main = scotty 8080 $ do
  middleware logStdoutDev
  -- middleware $ staticPolicy (noDots >-> addBase "static")

  get "/" $
    html $ startingPage

  get "/signup" $ do
    html signUpPage
