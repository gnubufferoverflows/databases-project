-- Evan's Elm Stdlib Extension: Json.Decode


module EESE.Json.Decode exposing (intDict)


import Dict exposing (Dict)
import EESE
import Json.Decode exposing (Decoder)


intDict : Decoder a -> Decoder (Dict Int a)
intDict decoder =
    Json.Decode.map (EESE.filterMapKeys String.toInt)
        <| Json.Decode.dict decoder
