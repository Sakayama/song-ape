module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    Int


init : Model
init =
    0



-- UPDATE


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1



-- VIEW


view : Model -> Html Msg
view model =
    div [] [viewPageHeader, viewWarnings, viewOrderContent]


viewPageHeader =
    div [ class "block__wrapper" ]
        [ div [ class "block__content page-header" ]
            [ h3 [ class "secondary-header" ]
                [ text "Order 45" ]
            ]
        ]


viewWarnings =
    div [ class "block__wrapper order-warnings" ]
        [ div [ class "block__content" ]
            [ p [ class "order-warnings__warning" ]
                [ text "The song “SONGTITLE” was purchased on “DATE.MONTH.YEAR”. We deleted\n          this item from your order." ]
            , p [ class "order-warnings__warning" ]
                [ text "The song “SONGTITLE” already exists on “ALBUMNAME” album. We deleted\n          this item from your order." ]
            ]
        ]


viewOrderContent =
    div [ class "block__wrapper" ]
        [ div [ class "block__content" ]
            [ viewAlbums
            , viewSongs
            , viewOrderSummary
            , viewConfButtons
            ]
        ]


viewAlbums =
    div [ class "albums" ]
        [ h3 [ class "secondary-header" ]
            [ text "Albums" ]
        , div [ class "albums__list" ]
            [ div [ class "removable-album" ]
                [ div [ class "album-item" ]
                    [ div [ class "album-item__img" ]
                        []
                    , span [ class "album-item__title" ]
                        [ text "Empty Sky" ]
                    , span [ class "album-item__year" ]
                        [ text "(1969)" ]
                    , div [ class "remove-button" ]
                        [ span [ class "remove-button__price" ]
                            [ text "$9.99" ]
                        ]
                    ]
                , img [ class "delete-icon", src "icons/delete.png" ]
                    []
                ]
            , div [ class "removable-album" ]
                [ div [ class "album-item" ]
                    [ div [ class "album-item__img" ]
                        []
                    , span [ class "album-item__title" ]
                        [ text "Blue Moves" ]
                    , span [ class "album-item__year" ]
                        [ text "(1976)" ]
                    , div [ class "remove-button" ]
                        [ span [ class "remove-button__price" ]
                            [ text "$9.99" ]
                        ]
                    ]
                , img [ class "delete-icon", src "icons/delete.png" ]
                    []
                ]
            , div [ class "removable-album" ]
                [ div [ class "album-item" ]
                    [ div [ class "album-item__img" ]
                        []
                    , span [ class "album-item__title" ]
                        [ text "A Single Man" ]
                    , span [ class "album-item__year" ]
                        [ text "(1978)" ]
                    , div [ class "remove-button" ]
                        [ span [ class "remove-button__price" ]
                            [ text "$9.99" ]
                        ]
                    ]
                , img [ class "delete-icon", src "icons/delete.png" ]
                    []
                ]
            ]
        ]


viewSongs =
    div [ class "songs" ]
        [ h3 [ class "secondary-header" ]
            [ text "Songs" ]
        , div [ class "song-with-number" ]
            [ img [ class "song-with-number__play-icon", src "icons/play.png" ]
                []
            , span [ class "song-with-number__title" ]
                [ text "Your Song" ]
            , span [ class "song-with-number__duration" ]
                [ text "9:00" ]
            , div [ class "remove-button" ]
                [ span [ class "remove-button__price" ]
                    [ text "$0.99" ]
                , img [ class "delete-icon", src "icons/delete.png" ]
                    []
                ]
            ]
        , div [ class "song-with-number" ]
            [ img [ class "song-with-number__play-icon", src "icons/play.png" ]
                []
            , span [ class "song-with-number__title" ]
                [ text "Goodbye Yellow Brick Road" ]
            , span [ class "song-with-number__duration" ]
                [ text "3:33" ]
            , div [ class "remove-button" ]
                [ span [ class "remove-button__price" ]
                    [ text "$0.99" ]
                , img [ class "delete-icon", src "icons/delete.png" ]
                    []
                ]
            ]
        , div [ class "song-with-number" ]
            [ img [ class "song-with-number__play-icon", src "icons/play.png" ]
                []
            , span [ class "song-with-number__title" ]
                [ text "Rocket Man" ]
            , span [ class "song-with-number__duration" ]
                [ text "6:10" ]
            , div [ class "remove-button" ]
                [ span [ class "remove-button__price" ]
                    [ text "$0.99" ]
                , img [ class "delete-icon", src "icons/delete.png" ]
                    []
                ]
            ]
        ]


viewOrderSummary =
    div [ class "order-summary" ]
        [ div [ class "order-summary__total secondary-header" ]
            [ text "Total: $88.04" ]
        , div [ class "order-summary__payment" ]
            [ div [ class "order-summary__payment-title" ]
                [ text "Payment Method" ]
            , div [ class "order-summary__payment-icons" ]
                [ div [ class "payment-method payment-method_selected" ]
                    [ img [ class "payment-method__icon", src "images/payment-visa.png" ]
                        []
                    ]
                , div [ class "payment-method" ]
                    [ img [ class "payment-method__icon", src "images/payment-mastercard.png" ]
                        []
                    ]
                , div [ class "payment-method" ]
                    [ img [ class "payment-method__icon", src "images/payment-paypal.png" ]
                        []
                    ]
                ]
            ]
        ]


viewConfButtons =
    div [ class "conf-buttons" ]
        [ div [ class "conf-buttons__button" ]
            [ img [ class "conf-buttons__button__icon", src "icons/arrow-left.png" ]
                []
            , span [ class "conf-buttons__button__label" ]
                [ text "Back" ]
            ]
        , div [ class "conf-buttons__button" ]
            [ span [ class "conf-buttons__button__label" ]
                [ text "Continue" ]
            , img [ class "conf-buttons__button__icon", src "icons/arrow-right.png" ]
                []
            ]
        ]
