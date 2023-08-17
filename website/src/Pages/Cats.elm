module Cat exposing (main)


import Crud
import Url.Builder


type alias Cat =
    { catID : Int
    , veterinarianID : Int
    , physicalDescription : String
    , weight : Float
    , age : Int
    , registratationDate : String
    , imageURL : Maybe String
    }


primaryEndpoint : Crud.Endpoint
primaryEndpoint =
    Url.Builder.absolute ["api", "cats"] []


main =
    Crud.main
        { read = Url.Builder.absolute ["api", "cats"]
        , create =
        , update = Url.Builder.absolute ["api", "cats"]
