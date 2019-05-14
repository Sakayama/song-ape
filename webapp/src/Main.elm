module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Data.Album exposing (Album)
import Data.Artist exposing (Artist)
import Data.Item exposing (AlbumDatagram, Item(..), SongDatagram, itemPrice, splitItems)
import Data.Order exposing (Order, OrderStatus(..), orderDecoder)
import Data.Song exposing (Song, showDuration)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Json.Decode as D exposing (Decoder, Error(..))


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { input : String
    , parsingResult : Result Error Order
    }


init : Model
init =
    { input = "", parsingResult = D.decodeString (D.fail "") "" }
        |> update (Change srcText)



--


srcText =
    """
{
  "orderId": 42,
  "orderStatus": { 
    "status": "unconfirmed",
    "warnings": ["warning 1", "warning 2"]
  },
  "items": [
    {
      "songs": [
        {
          "songId": "4",
          "artistId": "artist1",
          "albumId": "album1",
          "title": "Dark / Light",
          "duration": 510000
        },
        {
          "songId": "1",
          "artistId": "artist1",
          "albumId": "album1",
          "title": "Before the Beginning",
          "duration": 5100078
        }
      ],
      "artist": {
        "artistId": "artist1",
        "artistName": "John Frusciante",
        "artistPhoto": "https://media.gettyimages.com/photos/john-frusciante-of-red-hot-chili-peppers-performs-on-stage-at-palau-picture-id128104024?s=612x612",
        "artistDescription": "GOD111"
      },
      "album": {
        "albumId": "album1",
        "albumTitle": "Empyrean",
        "albumYear": 2009,
        "albumCover" : "https://upload.wikimedia.org/wikipedia/en/6/65/John_Frusciante_-_The_Empyrean.jpg",
        "albumDescription" : "here"
      },
      "price": 0.99
    },
    {
      "song": {
        "songId": "4",
        "artistId": "artist1",
        "albumId": "album1",
        "title": "Dark / Light",
        "duration": 510000
      },
      "artist": {
        "artistId": "artist1",
        "artistName": "John Frusciante",
        "artistPhoto": "https://media.gettyimages.com/photos/john-frusciante-of-red-hot-chili-peppers-performs-on-stage-at-palau-picture-id128104024?s=612x612",
        "artistDescription": "GOD111"
      },
      "album": {
        "albumId": "album1",
        "albumTitle": "Empyrean",
        "albumYear": 2009
      },
      "price": 0.99
    },   {
      "songs": [
        {
          "songId": "4",
          "artistId": "artist1",
          "albumId": "album1",
          "title": "Dark / Light",
          "duration": 510000
        },
        {
          "songId": "1",
          "artistId": "artist1",
          "albumId": "album1",
          "title": "Before the Beginning",
          "duration": 5100078
        }
      ],
      "artist": {
        "artistId": "artist1",
        "artistName": "John Frusciante",
        "artistPhoto": "https://media.gettyimages.com/photos/john-frusciante-of-red-hot-chili-peppers-performs-on-stage-at-palau-picture-id128104024?s=612x612",
        "artistDescription": "GOD111"
      },
      "album": {
        "albumId": "album1",
        "albumTitle": "Empyrean japanese edition deluxe",
        "albumYear": 2009,
        "albumCover" : "https://upload.wikimedia.org/wikipedia/en/6/65/John_Frusciante_-_The_Empyrean.jpg",
        "albumDescription" : "here"
      },
      "price": 0.99
    },
    {
      "song": {
        "songId": "4",
        "artistId": "artist1",
        "albumId": "album1",
        "title": "After the Ending",
        "duration": 510000
      },
      "artist": {
        "artistId": "artist1",
        "artistName": "John Frusciante",
        "artistPhoto": "https://media.gettyimages.com/photos/john-frusciante-of-red-hot-chili-peppers-performs-on-stage-at-palau-picture-id128104024?s=612x612",
        "artistDescription": "GOD111"
      },
      "album": {
        "albumId": "album1",
        "albumTitle": "Empyrean",
        "albumYear": 2009
      },
      "price": 0.99
    }
  ]
}

    """



-- UPDATE


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newInput ->
            { model | input = newInput, parsingResult = D.decodeString orderDecoder newInput }



-- VIEW


view : Model -> Html Msg
view model =
    div [] [ viewResult model.parsingResult ]


viewResult : Result Error Order -> Html Msg
viewResult parsingResult =
    case parsingResult of
        Ok x ->
            viewOrder x

        Err e ->
            div [] [ text <| D.errorToString e ]


viewOrder : Order -> Html Msg
viewOrder order =
    div []
        [ viewPageHeader order
        , viewWarnings order.orderStatus
        , viewOrderContent order
        ]


viewPageHeader : Order -> Html Msg
viewPageHeader order =
    div [ class "block__wrapper" ]
        [ div [ class "block__content page-header" ]
            [ h3 [ class "secondary-header" ]
                [ text ("Order " ++ String.fromInt order.orderId) ]
            , viewOrderStatus order.orderStatus
            ]
        ]


viewOrderStatus : OrderStatus -> Html Msg
viewOrderStatus orderStatus =
    case orderStatus of
        Unconfirmed warnings ->
            div []
                [ text "(unconfirmed)"
                , div [] (List.map text warnings)
                ]

        Confirmed timestamp ->
            String.fromInt timestamp |> text


viewWarning : String -> Html Msg
viewWarning warning =
  p [ class "order-warnings__warning" ]
    [ text warning ]


viewWarnings : OrderStatus -> Html Msg
viewWarnings orderStatus =
    case orderStatus of
        Unconfirmed warnings ->
            div [ class "block__wrapper order-warnings" ]
                [ div [ class "block__content" ] (List.map viewWarning warnings)
                ]

        Confirmed _ ->
            div [] []


viewOrderContent : Order -> Html Msg
viewOrderContent order =
    let
        ( songs, albums ) =
            splitItems order.items
    in
    div [ class "block__wrapper" ]
        [ div [ class "block__content" ]
            [ viewAlbums albums
            , viewSongs songs
            , viewOrderSummary order
            , viewConfButtons
            ]
        ]


viewAlbum : AlbumDatagram -> Html Msg
viewAlbum ad =
    div [ class "album-item" ]
        [ img [ class "album-item__img", src ad.album.albumCover ]
            []
        , span [ class "album-item__title" ]
            [ text ad.album.albumTitle ]
        , span [ class "album-item__year" ]
            [ text (String.fromInt ad.album.albumYear) ]
        , div [ class "remove-button" ]
            [ span [ class "remove-button__price" ]
                [ text ("$" ++ String.fromFloat ad.price) ]
            ]
        ]


viewRemoveableAlbum : AlbumDatagram -> Html Msg
viewRemoveableAlbum ad =
    div [ class "removable-album" ]
        [ viewAlbum ad
        , img [ class "delete-icon", src "icons/delete.png" ]
            []
        ]


viewAlbums : List AlbumDatagram -> Html Msg
viewAlbums albums =
    div [ class "albums" ]
        [ h3 [ class "secondary-header" ]
            [ text "Albums" ]
        , div [ class "albums__list" ]
            (List.map viewRemoveableAlbum albums)
        ]


viewRemovableSong : SongDatagram -> Html Msg
viewRemovableSong sd =
    div [ class "removable-song" ]
        [ img [ class "removable-song__play-icon", src "icons/play.png" ]
            []
        , span [ class "removable-song__title" ]
            [ text sd.song.title ]
        , span [ class "removable-song__duration" ]
            [ text (showDuration sd.song.duration) ]
        , div [ class "remove-button" ]
            [ span [ class "remove-button__price" ]
                [ text ("$" ++ String.fromFloat sd.price) ]
            , img [ class "delete-icon", src "icons/delete.png" ]
                []
            ]
        ]


viewSongs : List SongDatagram -> Html Msg
viewSongs songs =
    div [ class "songs" ]
        [ h3 [ class "secondary-header" ] [ text "Songs" ]
        , div [] (List.map viewRemovableSong songs)
        ]


viewOrderSummary : Order -> Html Msg
viewOrderSummary order =
    div [ class "order-summary" ]
        [ div [ class "order-summary__total secondary-header" ]
            [ order.items
                |> List.map itemPrice
                |> List.sum
                |> String.fromFloat
                |> (\x -> "Total: $" ++ x)
                |> text
            ]
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
