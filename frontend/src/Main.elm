module Main exposing (..)

import Types as T
import Page.Starting
import Page.SignUp

import Browser
import Element as El
import Element.Background as Background
import Element.Font as ElFont
import Html exposing (..)
import Html.Events exposing (onClick)

main : Program () T.Model T.Msg
main =
  Browser.element { init = init
                  , view = view
                  , update = update
                  , subscriptions = subscriptions
                  }


-- type Model
  -- = StartingPage
  -- | SignUpPage
  -- | UserPage
  -- | CoursePage

init : () -> (T.Model, Cmd T.Msg)
init _ =
  (T.StartingPage, Cmd.none)

subscriptions : T.Model -> Sub T.Msg
subscriptions _ =
  Sub.none

view : T.Model -> Html T.Msg
view model =
  let page =
        case model of
          T.StartingPage -> Page.Starting.startingPage
          T.SignUpPage   -> Page.SignUp.signUpPage
          _              -> Page.Starting.startingPage
  in
  El.layout
    [ Background.color (El.rgb255 149 117 205) 
    ] page

update : T.Msg -> T.Model -> (T.Model, Cmd T.Msg)
update msg model =
  case msg of
    T.GotSignUpPage -> (T.SignUpPage, Cmd.none)
    _               -> (T.StartingPage, Cmd.none) 



