
module View
  ( startingPage
  , signUpPage
  , signInPage
  ) where

import           Control.Monad                        (replicateM_)
import           Prelude                              hiding (head)

import           Data.Text.Lazy                       (Text)
import           Text.Blaze.Html.Renderer.Text
import           Text.Blaze.Html4.FrameSet            (center)
import           Text.Blaze.Html4.FrameSet.Attributes (action, bgcolor, charset,
                                                       method, name, type_,
                                                       value)
import           Text.Blaze.Html5

startingPage :: Text
startingPage = renderHtml $ do
  docTypeHtml $ do
    html $ do
      head $ do
        title "Main Page"
        meta ! charset "UTF-8"
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
      head $ do
        title "Sign Up"
        meta ! charset "UTF-8"
      body ! bgcolor "#BBCEDD" $ do
        replicateM_ 10 br
        center $ h3 "Sign Up"
        br
        center $ form ! method "post" ! action "/signup-post" $ do
          p "Username"
          input ! type_ "text" ! name "username"
          br
          p "Password"
          input ! type_ "password" ! name "password"
          br
          input ! type_ "radio" ! name "who" ! value "staff"
          " staff"
          br
          input ! type_ "radio" ! name "who" ! value "student"
          " student"
          br >> br
          button ! type_ "submit" $ "Sign Up"


signInPage :: Text
signInPage = renderHtml $ do
  docTypeHtml $ do
    html $ do
      head $ do
        title "Sign In"
        meta ! charset "UTF-8"
      body ! bgcolor "#BBCEDD" $ do
        replicateM_ 10 br
        center $ h3 "Sign In"
        br
        center $ form ! method "post" ! action "/signin-post" $ do
          p "Username"
          input ! type_ "text" ! name "username"
          br
          p "Password"
          input ! type_ "password" ! name "password"
          br >> br
          button ! type_ "submit" $ "Sign In"
