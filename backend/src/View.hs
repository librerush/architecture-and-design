
module View
  ( startingPage
  , signUpPage
  ) where

import           Control.Monad                        (replicateM_)

import           Data.Text.Lazy                       (Text)
import           Text.Blaze.Html.Renderer.Text
import           Text.Blaze.Html4.FrameSet            (center)
import           Text.Blaze.Html4.FrameSet.Attributes (action, bgcolor, method,
                                                       type_, value)
import           Text.Blaze.Html5

startingPage :: Text
startingPage = renderHtml $ do
  docTypeHtml $ do
    html $ do
      body ! bgcolor "#BBCEDD" $ do
        replicateM_ 10 br
        center $ h2 "Distance Learning"
        br
        center $ form ! method "get" ! action "/signup" $ do
          input ! type_ "submit" ! value "Sign Up"
        br
        center $ form ! method "get" ! action "/signin" $ do
          input ! type_ "submit" ! value "Sign In"
        br

signUpPage :: Text
signUpPage = renderHtml $ do
  docTypeHtml $ do
    html $ do
      body ! bgcolor "#BBCEDD" $ do
        replicateM_ 10 br
        center $ h3 "Sign Up"
        br
