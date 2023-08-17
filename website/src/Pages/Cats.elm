module Pages.Cats exposing (main)


import Crud
import Dict exposing (Dict)
import EESE
import Json.Decode as Dec exposing (Decoder)
import Json.Encode as Enc exposing (Value)
import Url.Builder


type alias Cat =
    { veterinarianID : Int
    , physicalDescription : String
    , weight : Float
    , age : Int
    , registrationDate : String
    }


primaryEndpoint : Crud.Endpoint
primaryEndpoint =
    Url.Builder.absolute ["api", "cats"] []


subqueries : Dict String Crud.Endpoint
subqueries =
    Dict.fromList [ ( "veterinarians", Url.Builder.absolute ["api", "vets", "partial"] [] ) ]


columns : List (Crud.Column Cat)
columns =
    [ { header = "Name"
      , view = Crud.Projection (always "Haskell") Nothing
      }
    , { header = "Age"
      , view = Crud.Projection (.age >> String.fromInt) <| Just ( "age", Crud.Input <| String.toInt >> EESE.isJust )
      }
    , { header = "Weight"
      , view = Crud.Projection (.weight >> String.fromFloat) <| Just ( "weight", Crud.Input <| String.toFloat >> EESE.isJust )
      }
    , { header = "Registration Date"
      , view = Crud.Projection .registrationDate <| Just ( "registrationDate", Crud.Date )
      }
    , { header = "Physical Description"
      , view = Crud.Projection .physicalDescription <| Just ( "physicalDescription", Crud.TextArea )
      }
    , { header = "Veterinarian"
      , view = Crud.Subquery "veterinarians" .veterinarianID <| Just "veterinarianID"
      }
    ]


decoder : Decoder Cat
decoder =
    Dec.map5 Cat
        (Dec.field "veterinarianID" Dec.int)
        (Dec.field "physicalDescription" Dec.string)
        (Dec.field "weight" Dec.float)
        (Dec.field "age" Dec.int)
        (Dec.field "registrationDate" Dec.string)


encoder : Cat -> Value
encoder cat =
    Enc.object
        [ ( "veterinarianID", Enc.int cat.veterinarianID )
        , ( "physicalDescription", Enc.string cat.physicalDescription )
        , ( "weight", Enc.float cat.weight )
        , ( "age", Enc.int cat.age )
        , ( "registrationDate", Enc.string cat.registrationDate )
        ]


main : Program () (Crud.Model Cat) (Crud.Msg Cat)
main =
    Crud.app
        { read = primaryEndpoint
        , create = primaryEndpoint
        , edit = primaryEndpoint
        , delete = primaryEndpoint
        , subqueries = subqueries
        , columns = columns
        , decoder = decoder
        , encoder = encoder
        }
