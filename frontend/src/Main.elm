module Main exposing (..)

import Types as T
import StartingPage

import Browser
import Element as El
import Element.Background as Background
import Element.Font as ElFont
import Html exposing (..)
import Html.Events exposing (onClick)

main : Program () T.Model msg
main =
  Browser.element { init = init
                  , view = view
                  , update = update
                  , subscriptions = subscriptions
                  }


--type Model
  --= StartingPage
  --| SignUpPage
  --| UserPage
  --| CoursePage

init : () -> (T.Model, Cmd msg)
init _ =
  (T.StartingPage, Cmd.none)

subscriptions : T.Model -> Sub msg
subscriptions _ =
  Sub.none

view : T.Model -> Html msg
view model =
  let page =
        case model of
          T.StartingPage -> StartingPage.startingPage
          _              -> StartingPage.startingPage
  in
  El.layout
    [ Background.color (El.rgb255 149 117 205) 
    ] page

update : msg -> T.Model -> (T.Model, Cmd msg)
update msg model =
  (T.StartingPage, Cmd.none)



