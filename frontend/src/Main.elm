module Main exposing (..)

import Types

import Browser
import Element as El
import Html
import Html.Events exposing (onClick)


main =
  Browser.element { init = init
                  , view = view
                  , update = update
                  , subscriptions = subscriptions
                  }


type Model = Model

subscriptions : Model -> Sub msg
subscriptions _ =
  Sub.none

view : Model -> Html msg
view model =
 


