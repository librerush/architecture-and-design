
module Page.SignUp exposing
  (signUpPage)

import Types as T

import Element as El
import Element.Font as Font
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Element.Region as Region


newPassword : El.Element T.Msg
newPassword =
  Input.newPassword
    [ El.below newPasswordRepeat ]
    { onChange = (\x -> T.GotPassword x)
    , text = ""
    , placeholder = Nothing
    , label = Input.labelAbove [] (El.text "\npassword")
    , show = False
    }


newPasswordRepeat : El.Element T.Msg
newPasswordRepeat =
  Input.newPassword
    [ El.spacing 15
    , El.padding 15
    , El.below signUpButton
    ]
    { onChange = (\x -> T.GotNoAction)
    , text = ""
    , placeholder = Nothing
    , label = Input.labelAbove [] (El.text "\nrepeat password")
    , show = False
    }


signUpPage : El.Element T.Msg
signUpPage =
  El.row
    [ El.centerX
    , El.centerY
    , El.spacing 5
    , El.padding 5
    ]
    [ (Input.username [ El.below newPassword ]
                         { onChange = (\x -> T.GotUserName x)
                         , text = ""
                         , placeholder = Nothing
                         , label = Input.labelAbove [] (El.text "username")
                         })
    ]


signUpButton =
    Input.button
        [ Background.color <| El.rgb255 224 224 224
        , El.padding 2
        , El.spacing 5
        ]
        { onPress = Nothing
        , label = El.text "Sign Up"
        }

