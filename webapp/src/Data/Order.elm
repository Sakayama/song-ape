module Data.Order exposing (Order, OrderStatus(..), orderDecoder)

import Data.Item exposing (Item(..), itemDecoder)
import Json.Decode as D exposing (Decoder)


type OrderStatus
    = Unconfirmed
    | Confirmed Int


type alias Order =
    { orderId : Int, items : List Item, orderStatus : OrderStatus }


filter : (a -> Bool) -> String -> Decoder a -> Decoder a
filter f errorMsg =
    D.andThen
        (\x ->
            if f x then
                D.succeed x

            else
                D.fail errorMsg
        )


orderDecoder : Decoder Order
orderDecoder =
    D.map3 Order
        (D.field "orderId" D.int)
        (D.field "items" (D.list itemDecoder))
        (D.field "orderStatus" orderStatusDecoder)


orderStatusDecoder : Decoder OrderStatus
orderStatusDecoder =
    D.oneOf
        [ unconfirmedDecoder
        , confirmedDecoder
        ]


unconfirmedDecoder : Decoder OrderStatus
unconfirmedDecoder =
    D.field "status" D.string
        |> filter (\s -> s == "unconfirmed") "not unconfirmed"
        |> D.map (\_ -> Unconfirmed)


confirmedDecoder : Decoder OrderStatus
confirmedDecoder =
    D.field "status" D.string
        |> filter (\s -> s == "confirmed") "not confirmed"
        |> D.map (\_ -> Confirmed 424242)
