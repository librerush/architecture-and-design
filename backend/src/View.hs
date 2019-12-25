
module View
  ( addLecturePage
  , createCoursePage
  , startingPage
  , signUpPage
  , signInPage
  , homeStudentPage
  , homeStaffPage
  , coursePage
  ) where


import           Model

import           Control.Monad                        (replicateM_, forM_)
import           Data.Monoid                          ((<>))
import           Prelude                              hiding (div, head)

import           Data.Text.Lazy                       (Text, pack, toStrict)
import           Text.Blaze                           (textValue)
import           Text.Blaze.Html.Renderer.Text
import           Text.Blaze.Html4.FrameSet            (center)
import           Text.Blaze.Html4.FrameSet.Attributes (action, bgcolor, charset,
                                                       cols, href, method, name,
                                                       rows, src, style, type_,
                                                       value)
import           Text.Blaze.Html5                     hiding (style)


createCoursePage :: Text
createCoursePage = renderHtml $ do
  docTypeHtml $ do
    html $ do
      head $ do
        title "Creating a new course"
        meta ! charset "UTF-8"
      body ! bgcolor "#BBCEDD" $ do
        header ! style "border-bottom: 0.2rem; color: #555;" $ do
          nav ! style "text-align: center; margin: 0 auto 3rem;" $ do
            let aStyle = "text-transform: uppercase; \
              \display: inline; margin: 0 0.6rem;"
            a ! style aStyle ! href "/" $ "main"
            a ! style aStyle ! href "/home-staff" $ "home"
            a ! style aStyle ! href "/log-out" $ "log out"

        replicateM_ 1 br
        center $ do
          form ! method "post" ! action "/create/new_course" $ do
            p ! style "font-style: italic" $ "name of course"
            input ! type_ "text" ! name "course_name"
            p ! style "font-style: italic" $ "description of course"
            input ! type_ "text" ! name "course_desc"
            replicateM_ 2 br
            button ! type_ "submit" $ "create"


addLecturePage :: Text -> Text
addLecturePage cname = renderHtml $ do
  docTypeHtml $ do
    html $ do
      head $ do
        title $ "Add lecture | " <> (toHtml cname)
        meta ! charset "UTF-8"
      body ! bgcolor "#BBCEDD" $ do
        header ! style "border-bottom: 0.2rem; color: #555;" $ do
          nav ! style "text-align: center; margin: 0 auto 3rem;" $ do
            let aStyle = "text-transform: uppercase; \
              \display: inline; margin: 0 0.6rem;"
            a ! style aStyle ! href "/" $ "main"
            a ! style aStyle ! href "/home-staff" $ "home"
            a ! style aStyle ! href "/log-out" $ "log out"

        replicateM_ 1 br
        center $ do
          p ! style "font-style: bold;" $ "add a lecture for " <> toHtml cname
          form ! method "post" !
            action ("/add/lecture/content/" <> (textValue $ toStrict cname)) $ do
              textarea ! name "lec_text" ! rows "20" ! cols "35" $ ""
              replicateM_ 2 br
              button ! type_ "submit" $ "add"


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


homeStudentPage :: Student -> [Text] -> Text
homeStudentPage !student !courses = renderHtml $ do
  docTypeHtml $ do
    html $ do
      head $ do
        title "Home Page"
        meta ! charset "UTF-8"
      body ! bgcolor "#BBCEDD" $ do
        header ! style "border-bottom: 0.2rem; color: #555;" $ do
          nav ! style "text-align: center; margin: 0 auto 3rem;" $ do
            let aStyle = "text-transform: uppercase; \
              \display: inline; margin: 0 0.6rem;"
            a ! style aStyle ! href "/" $ "main"
            a ! style aStyle ! href "/home-student" $ "home"
            a ! style aStyle ! href "/log-out" $ "log out"

        replicateM_ 1 br
        center $ do
          p ! style "font-style: italic;" $ toHtml $ nameStudent student
          img ! style "width: 100px; height: 100px;" ! src "/avatar.png"
          replicateM_ 2 br
          forM_ courses $
            \course -> a ! style "font-style: bold;" !
              href ("/course/" <> (textValue $ toStrict course)) $
              "[" <> (toHtml course) <> "]"


homeStaffPage :: Staff -> Text
homeStaffPage !staff = renderHtml $ do
  docTypeHtml $ do
    html $ do
      head $ do
        title "Home Page"
        meta ! charset "UTF-8"
      body ! bgcolor "#BBCEDD" $ do
        header ! style "border-bottom: 0.2rem; color: #555;" $ do
          nav ! style "text-align: center; margin: 0 auto 3rem;" $ do
            let aStyle = "text-transform: uppercase; \
              \display: inline; margin: 0 0.6rem;"
            a ! style aStyle ! href "/" $ "main"
            a ! style aStyle ! href "/home-staff" $ "home"
            a ! style aStyle ! href "/log-out" $ "log out"

        replicateM_ 1 br
        center $ do
          p ! style "font-style: italic;" $ toHtml $ nameStaff staff
          img ! style "width: 100px; height: 100px;" ! src "/avatar.png"
          replicateM_ 2 br
          form ! method "post" ! action "/create/course" $ do
            button ! type_ "submit" $ "create a course"


coursePage :: Course -> Text
coursePage !course = renderHtml $ do
  docTypeHtml $ do
    html $ do
      head $ do
        title $ toHtml $ nameCourse course
        meta ! charset "UTF-8"
      body ! bgcolor "#BBCEDD" $ do
        header ! style "border-bottom: 0.2rem; color: #555;" $ do
          nav ! style "text-align: center; margin: 0 auto 3rem;" $ do
            let aStyle = "text-transform: uppercase; \
              \display: inline; margin: 0 0.6rem;"
            a ! style aStyle ! href "/" $ "main"
            a ! style aStyle ! href "/home-student" $ "home"
            a ! style aStyle ! href "/log-out" $ "log out"

        replicateM_ 1 br
        center $ do
          let !content_ = decodeCourseAdd $ materialsCourse course
          case content_ of
            Just content ->
              p ! style "font-style: italic;" $ toHtml $ caddLec content
            Nothing -> p "..."


