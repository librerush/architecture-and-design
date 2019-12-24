
module View
  ( startingPage
  , signUpPage
  , signInPage
  , homeStudentPage
  , homeStaffPage
  ) where


import           Model

import           Control.Monad                        (replicateM_)
import           Data.Monoid                          ((<>))
import           Prelude                              hiding (div, head)

import           Data.Text.Lazy                       (Text, pack)
import           Text.Blaze.Html.Renderer.Text
import           Text.Blaze.Html4.FrameSet            (center)
import           Text.Blaze.Html4.FrameSet.Attributes (action, bgcolor, charset,
                                                       method, name, style,
                                                       type_, value)
import           Text.Blaze.Html5                     hiding (style)

startingPage :: Text
startingPage = renderHtml $ do
  docTypeHtml $ do
    html $ do
      head $ do
        title "Main Page"
        meta ! charset "UTF-8"
      body ! bgcolor "#BBCEDD" $ do
        replicateM_ 10 br
        center ! style "color:#303F9F; font-family: Arial;" $
          h2 "Distance Learning"
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
        center ! style "color:#303F9F; font-family: Arial;" $ h3 "Sign Up"
        br
        center $ form ! method "post" ! action "/signup-post" $ do
          p ! style "font-style: italic" $ "Username"
          input ! type_ "text" ! name "username"
          br
          p ! style "font-style: italic" $ "Password"
          input ! type_ "password" ! name "password"
          br
          p ! style "font-style: italic" $ "Repeat password"
          input ! type_ "password" ! name "repeatpasswd"
          br
          input ! type_ "radio" ! name "who" ! value "staff"
          " staff"
          br
          input ! type_ "radio" ! name "who" ! value "student"
          "   student"
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
        center ! style "color:#303F9F; font-family: Arial;" $ h3 "Sign In"
        br
        div ! style "text-align: center;" $
          form ! method "post" ! action "/signin-post" $ do
            p ! style "font-style: italic" $ "Username"
            input ! type_ "text" ! name "username"
            br
            p ! style "font-style: italic" $ "Password"
            input ! type_ "password" ! name "password"
            br >> br
            button ! type_ "submit" $ "Sign In"

homeStudentPage :: Student -> Text
homeStudentPage !student = renderHtml $ do
  docTypeHtml $ do
    html $ do
      head $ do
        title "Home Page"
        meta ! charset "UTF-8"
      body ! bgcolor "#BBCEDD" $ do
        replicateM_ 10 br
        center $ p $ toHtml $ "name: " <> nameStudent student
        br
        center $ p $ toHtml $ ("id: " :: Text) <>
          (pack . show $ idStudent student)

homeStaffPage :: Staff -> Text
homeStaffPage !staff = renderHtml $ do
  docTypeHtml $ do
    html $ do
      head $ do
        title "Home Page"
        meta ! charset "UTF-8"
      body ! bgcolor "#BBCEDD" $ do
        replicateM_ 10 br
        center $ p $ toHtml $ "name: " <> nameStaff staff
        br
        center $ p $ toHtml $ ("id: " :: Text) <>
          (pack . show $ idStaff staff)
