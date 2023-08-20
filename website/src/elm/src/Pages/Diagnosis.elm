module Pages.Diagnosis exposing (main)


import Crud


type alias Diagnosis =
    { diagnosisDate : String
    }


type alias DiagnosisKey =
    { catID : Int
    , diseaseID : Int
    }


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
        [ ( "catID", Url.Builder.absolute ["api", "cats"] [] )
        , ( "diseaseID", Url.Builder.absolute ["api", "cats"] [] )
        ]


columns : List (Crud.Column DiagnosisKey Diagnosis)
columns =
    [ { header = "Cat IDs"
      , view = Crud.Id <| .catID >> String.fromInt
      }
    , { header = "Disease IDs"
      , view = Crud.Id <| .diseaseID >> String.fromInt
      }
    , { header = "Diagnosis Date"
      , view = Crud.Projection .diagnosisDate "diagnosisDate" Crud.Date
      }
    ]


keyDecoder : Decoder DiagnosisKey
keyDecoder =

