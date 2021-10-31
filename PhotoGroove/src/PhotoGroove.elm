module PhotoGroove exposing (..)

import Html exposing (div, h1, img, text)
import Html.Attributes exposing (..)


view model =
    div [ class "photo-groove" ]
        [ h1 [] [ text "Photo Groove" ]
        , div [ id "thumbnails" ]
            [ img [ src "http://elm-in-action.com/1.jpeg" ] []
            , img [ src "http://elm-in-action.com/2.jpeg" ] []
            , img [ src "http://elm-in-action.com/3.jpeg" ] []
            ]
        ]


main =
    view "no model yet"
