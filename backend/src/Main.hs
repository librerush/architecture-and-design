{-# LANGUAGE OverloadedStrings #-}
import           Web.Scotty

import           Data.Monoid (mconcat)

main = scotty 8080 $
  get "/:course" $ do
  beam <- param "course"
  html $ mconcat ["<h1>Welcome to the ", beam, "!</h1>"]

