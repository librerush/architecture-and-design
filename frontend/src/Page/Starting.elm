
module Page.Starting exposing (startingPage)

import Types as T

import Element as El
import Element.Font as Font
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input

dlText : El.Element T.Msg
dlText =
  El.el
    [ Font.size 30
    , Font.family [ Font.sansSerif ]
    , Font.color (El.rgb255 250 250 250)
    , Border.widthXY 10 10
    , Border.dashed
    --, El.spacing 100
    , El.padding 80
    , El.below signInButton
    , El.below signUpButton
    ]
    (El.text "Distance Learning")

signInButton : El.Element T.Msg
signInButton =
  Input.button
    [ --Background.color (El.rgb255 250 250 250)
      El.focused [ Background.color (El.rgb255 121 134 203)]
    , El.padding 1
    , El.alignLeft
    , El.height (El.px 100)
    , El.width (El.px 100)
    ]
    { onPress = Nothing
    , label = El.text "Sign In"
    }

signUpButton : El.Element T.Msg
signUpButton =
  Input.button
    [ --Background.color (El.rgb255 250 250 250)
      El.focused [ Background.color (El.rgb255 121 134 203)]
    , El.padding 1
    , El.alignRight
    , El.height (El.px 100)
    , El.width (El.px 100)
    ]
    { onPress = Just (T.GotSignUpPage)
    , label = El.text "Sign Up"
    }

startingPage : El.Element T.Msg
startingPage =
  El.row
    [ El.centerX
    , El.centerY
    , Background.color (El.rgb255 121 134 203)
    , El.padding 80
    , El.spacing 20
    ]
    [ dlText
    ]


