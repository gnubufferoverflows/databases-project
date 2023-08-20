module Main exposing (main)


import Browser


import Pages.Home as Home
import Pages.Cats as Cats


type alias Flags =
    ()


type Model
    = Home Home.Model
    | Cats Cats.Model


type alias Msg
    = Home Home.Msg
    | Cats Cats.Msg


init : Flags -> ( Model, Cmd Msg )
init =
    always ()


-- pretty boilerplatey
view : Model -> Document Msg
view model =
    case model of
        Home hm ->
            Home.view hm

        Cats cm ->
            Cats.view cm


update : Msg -> Model -> ( Model, Cmd Msg )
update 
