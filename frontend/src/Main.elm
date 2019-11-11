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



init : () -> (T.Model, Cmd T.Msg)
init _ =
  (T.initModel, Cmd.none)


subscriptions : T.Model -> Sub T.Msg
subscriptions _ =
  Sub.none


view : T.Model -> Html T.Msg
view model =
  let page =
        case model.page of
          T.StartingPage -> Page.Starting.startingPage
          T.SignUpPage   -> Page.SignUp.signUpPage
          _              -> Page.Starting.startingPage
  in
  El.layout
    [ Background.color (El.rgb255 149 117 205) ] page

-- | Updates Student's or Staff's name 
updateName : T.Model -> String -> T.Model
updateName model uname =
  case model.who of
    T.WhoStudent s ->
      let whoStudent = T.WhoStudent {s | name = uname}
      in
          {model | who = whoStudent}
    T.WhoStaff s   ->
      let whoStaff = T.WhoStaff {s | name = uname}
      in
          {model | who = whoStaff}
    _              -> model


update : T.Msg -> T.Model -> (T.Model, Cmd T.Msg)
update msg model =
  case msg of
    T.GotSignUpPage -> ({model | page = T.SignUpPage}, Cmd.none)
    T.GotNoAction   -> (model, Cmd.none)
    T.GotPassword passwd -> ({model | password = passwd}, Cmd.none)
    T.GotUserName uname  -> (updateName model uname, Cmd.none)
    _               -> (model, Cmd.none) 



