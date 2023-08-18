-- Evan's Elm Stdlib Extension: Json.Decode


module EESE.Json.Decode exposing
    ( tuple
    , intDict
    , arbitraryDict
    , decodeValueMaybe
    )


import Dict exposing (Dict)
import EESE
import Json.Decode exposing (Decoder, Value)


tuple : Decoder a -> Decoder b -> Decoder ( a, b )
tuple first second =
    Json.Decode.map2 Tuple.pair
        (Json.Decode.index 0 first)
        (Json.Decode.index 1 second)


arbitraryDict : Decoder comparable -> Decoder a -> Decoder (Dict comparable a)
arbitraryDict keyDecoder valueDecoder =
    Json.Decode.map Dict.fromList
        <| Json.Decode.list (tuple keyDecoder valueDecoder)


intDict : Decoder a -> Decoder (Dict Int a)
intDict =
    arbitraryDict Json.Decode.int


decodeValueMaybe : Decoder a -> Value -> Maybe a
decodeValueMaybe decoder =
    Json.Decode.decodeValue decoder >> Result.toMaybe
