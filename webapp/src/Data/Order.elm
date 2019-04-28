module Data.Order exposing (Order, orderDecoder)

import Data.Item exposing (Item(..), itemDecoder)
import Json.Decode as D exposing (Decoder)


type alias Order =
    { orderId : Int, items : List Item }


orderDecoder : Decoder Order
orderDecoder =
    D.map2 Order
        (D.field "orderId" D.int)
        (D.field "items" (D.list itemDecoder))
