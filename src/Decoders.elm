module Decoders exposing (..)

import Types exposing (..)
import Json.Decode exposing (..)


payloadDecoder : Decoder Payload
payloadDecoder =
    map2 Payload
        (at [ "cityName" ] string)
        (at [ "countryCode" ] string)


itemDecoder : Decoder Item
itemDecoder =
    map2 Item
        (at [ "description" ] string)
        (at [ "icon" ] string)


dayDecoder : Decoder Day
dayDecoder =
    map3 Day
        (at [ "dt" ] int)
        (at [ "dt_txt" ] string)
        (at [ "weather" ] (list itemDecoder))


weatherDecoder : Decoder Weather
weatherDecoder =
    map2 Weather
        (at [ "city", "name" ] string)
        (at [ "list" ] (list dayDecoder))
