-- the cat page


module Pages.Cats exposing
    ( Flags
    , Model
    , Msg
    , init
    , view
    , update
    , sub
    )


import Browser
import Dict exposing (Dict)
import Json.Decode as Dec exposing (Decoder)
import Json.Encode as Enc exposing (Value)
import Url.Builder


import Crud
import EESE


type alias Cat =
    { veterinarianID : Int
    , physicalDescription : String
    , weight : Float
    , age : Int
    , registrationDate : String
    }


type alias Flags =
    Crud.Flags


type alias Model =
    Crud.Model Int Cat


type alias Msg =
    Crud.Msg Int Cat


primaryEndpoint : Crud.Endpoint
primaryEndpoint =
    Url.Builder.absolute ["api", "cats"] []


subqueries : Dict String Crud.Endpoint
subqueries =
    Dict.fromList [ ( "veterinarians", Url.Builder.absolute ["api", "vets", "partial"] [] ) ]


columns : List (Crud.Column Int Cat)
columns =
    [ { header = "Name"
      , view = Crud.Preset "Haskell"
      }
    , { header = "Id"
      , view = Crud.Id String.fromInt
      }
    , { header = "Age"
      , view = Crud.Projection (.age >> String.fromInt) "age" <| Crud.Input <| String.toInt >> EESE.isJust
      }
    , { header = "Weight"
      , view = Crud.Projection (.weight >> String.fromFloat) "weight" <| Crud.Input <| String.toFloat >> EESE.isJust
      }
    , { header = "Registration Date"
      , view = Crud.Projection .registrationDate "registrationDate" Crud.Date
      }
    , { header = "Physical Description"
      , view = Crud.Projection .physicalDescription "physicalDescription" Crud.TextArea
      }
    , { header = "Veterinarian"
      , view = Crud.Subquery "veterinarians" .veterinarianID "veterinarianID"
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


app : Crud.AppType Model Msg
app =
    Crud.app
        { read = primaryEndpoint
        , create = primaryEndpoint
        , edit = primaryEndpoint
        , delete = primaryEndpoint
        , subqueries = subqueries
        , columns = columns
        , keyDecoder = Dec.int
        , valueDecoder = decoder
        , keyEncoder = Enc.int
        , valueEncoder = encoder
        , customizableIdValues = []
        }


init : () -> ( Model, Cmd Msg )
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
