-- module that puts all of the pages together
-- written by Evan
-- this is the boilerplate file


module Main exposing (main)


import Browser
import Url exposing (Url)


import EESE
import Route


import Pages.Home as Home
import Pages.Cats as Cats


type alias Flags =
    ()


type alias Model =
    { key : Nav.Key
    , page : Page
    }


type Page
    = Home Home.Model
    | Cats Cats.Model
    | Diagnoses Diagnoses.Model


type alias Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | Foreign Foreign


type Foreign
    = HomeMsg Home.Msg
    | CatsMsg Cats.Msg
    | DiagnosesMsg Diagnoses.Msg


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ key =
    let
        ( initModel, initCmd ) =
            Home.init ()
    in
        ( { key = key
          , initModel = Home initModel
          }
        , initCmd
        )


-- pretty boilerplatey
view : Model -> Document Msg
view model =
    EESE.uncurry EESE.Browser.mapDocument
        <| Tuple.mapFirst ((<<) Foreign)
        <| case model.page of
            Home hm ->
                ( HomeMsg, Home.view hm )

            Cats cm ->
                ( CatsMsg, Cats.view cm )

            Diagnoses dm ->
                ( DiagnosesMsg, Diagnoses.view dm )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked req ->
            case req of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key <| Url.toString url )

                Browser.External url ->
                    ( model, Nav.load url )

        UrlChanged url ->
            let
                ( page, pageInit ) =
                    case Route.fromUrl url of
                        Route.Home ->
                            ( Home, HomeMsg, Home.init () )

                        Route.Cats ->
                            ( Cats, CatsMsg, Cats.init () )

                        Route.Diagnoses ->
                            ( Diagnoses, DiagnosesMsg, Diagnoses.init () )
            in
                model |>
                    start page pageInit

        Foreign fmsg ->


handleForeignMsg :
    (model -> Page)
    -> (msg -> Foreign)
    -> (msg -> model -> ( model, Cmd msg ))
    -> msg
    -> model
    -> Model
    -> ( Model, Cmd Msg )
handleForeignMsg page msg update fmsg fmodel model =
    start page msg (update fmsg fmodel) model


start : (model -> Page) -> (msg -> Msg) -> ( model, Cmd msg ) -> Model -> ( Model, Cmd Msg )
start page msg ( newModel, cmd ) model =
    ( { model
      | page = page newModel
      }
    , Cmd.map msg cmd
    )


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
