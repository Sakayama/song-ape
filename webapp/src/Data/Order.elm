module Data.Order exposing (Order, OrderStatus(..), orderDecoder)

import Data.Item exposing (Item(..), itemDecoder)
import Json.Decode as D exposing (Decoder)

-- unconfirmed contains a list of warnings
type OrderStatus
    = Unconfirmed (List String)
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
    D.map2 (\_ warnings -> Unconfirmed warnings)
        unconfirmedStatusDecoder
        (D.field "warnings" (D.list D.string))


unconfirmedStatusDecoder : Decoder String
unconfirmedStatusDecoder =
    D.field "status" D.string
        |> filter (\s -> s == "unconfirmed") "not unconfirmed"


confirmedDecoder : Decoder OrderStatus
confirmedDecoder =
    D.map2 (\_ timestamp -> Confirmed timestamp)
        confirmedStatusDecoder
        (D.field "timestamp" D.int)


confirmedStatusDecoder : Decoder String
confirmedStatusDecoder =
    D.field "status" D.string
        |> filter (\s -> s == "confirmed") "not confirmed"
