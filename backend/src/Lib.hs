
module Lib
  (main)
  where

import           View

import           Control.Monad.IO.Class

import           Data.ByteString                      (ByteString)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.Wai.Middleware.Static        (addBase, noDots,
                                                       staticPolicy, (>->))
import           Web.Scotty



main :: IO ()
main = scotty 8080 $ do
  middleware logStdoutDev
  -- middleware $ staticPolicy (noDots >-> addBase "static")

  get "/" $
    html startingPage

  get "/signup" $ do
    html signUpPage

  get "/signin" $ do
    html signInPage
