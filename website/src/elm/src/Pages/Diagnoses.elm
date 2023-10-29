-- Diagnoses CRUD page
-- By Evan


module Pages.Diagnoses exposing
    ( Model
    , Msg
    , init
    , view
    , update
    , subscriptions
    )


import Browser
import Dict exposing (Dict)
import Json.Decode as Dec exposing (Decoder, Value)
import Json.Encode as Enc
import Url.Builder


import Crud


type alias Flags =
    Crud.Flags


type alias Diagnosis =
    { diagnosisDate : String
    }


type alias DiagnosisKey =
    ( Int, Int )


type alias Model =
    Crud.Model DiagnosisKey Diagnosis


type alias Msg =
    Crud.Msg DiagnosisKey Diagnosis


primaryEndpoint : Crud.Endpoint
primaryEndpoint =
    Url.Builder.absolute ["api", "diagnosis"] []


subqueries : Dict String Crud.Endpoint
subqueries =
    Dict.fromList
        [ ( "cats", Url.Builder.absolute ["api", "cats"] [] )
        , ( "diseases", Url.Builder.absolute ["api", "cats"] [] )
        ]


columns : List (Crud.Column DiagnosisKey Diagnosis)
columns =
    [ { header = "Cat IDs"
      , view = Crud.Id <| Tuple.first >> String.fromInt
      }
    , { header = "Disease IDs"
      , view = Crud.Id <| Tuple.second >> String.fromInt
      }
    , { header = "Diagnosis Date"
      , view = Crud.Projection .diagnosisDate "diagnosisDate" Crud.Date
      }
    ]


keyDecoder : Decoder DiagnosisKey
keyDecoder =
    Dec.map2 Tuple.pair
        (Dec.field "catID" Dec.int)
        (Dec.field "diseaseID" Dec.int)


valueDecoder : Decoder Diagnosis
valueDecoder =
    Dec.map Diagnosis
        (Dec.field "diagnosisDate" Dec.string)
    


keyEncoder : DiagnosisKey -> Value
keyEncoder diagnosisKey =
    Enc.object
        [ ( "catID", Enc.int <| Tuple.first diagnosisKey )
        , ( "diseaseID", Enc.int <| Tuple.second diagnosisKey )
        ]


valueEncoder : Diagnosis -> Value
valueEncoder diagnosis =
    Enc.object
        [ ( "diagnosisDate", Enc.string diagnosis.diagnosisDate ) ]


customizableIdValues : List Crud.CustomizableIdValue
customizableIdValues =
    [ { label = "Cat"
      , subquery = "cats"
      , interfaceKey = "catID"
      }
    , { label = "Disease"
      , subquery = "diseases"
      , interfaceKey = "diseaseID"
      }
    ]


title : String
title =
    "Diagnoses"


app : Crud.AppType Flags Model Msg
app =
    Crud.app
        { read = primaryEndpoint
        , create = primaryEndpoint
        , edit = primaryEndpoint
        , delete = primaryEndpoint
        , subqueries = subqueries
        , columns = columns
        , keyDecoder = keyDecoder
        , valueDecoder = valueDecoder
        , keyEncoder = keyEncoder
        , valueEncoder = valueEncoder
        , customizableIdValues = customizableIdValues
        , title = title
        }


init : Flags -> ( Model, Cmd Msg )
init =
    app.init


view : Model -> Browser.Document Msg
view =
    app.view


update : Msg -> Model -> ( Model, Cmd Msg )
update =
    app.update


subscriptions : Model -> Sub Msg
subscriptions =
    app.subscriptions
