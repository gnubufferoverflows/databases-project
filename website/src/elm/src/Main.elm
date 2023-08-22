-- module that puts all of the pages together
-- written by Evan
-- this is the boilerplate file


module Main exposing (main)


import Browser
import Browser.Navigation as Nav
import Url exposing (Url)


import EESE
import EESE.Browser
import Route


import Pages.Home as Home
import Pages.Cats as Cats
import Pages.Diagnoses as Diagnoses


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


type Msg
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
          , page = Home initModel
          }
        , Cmd.map (Foreign << HomeMsg) initCmd
        )


-- pretty boilerplatey
view : Model -> Browser.Document Msg
view model =
    case model.page of
        Home hm ->
            EESE.Browser.mapDocument (Foreign << HomeMsg) <| Home.view hm

        Cats cm ->
            EESE.Browser.mapDocument (Foreign << CatsMsg) <| Cats.view cm

        Diagnoses dm ->
            EESE.Browser.mapDocument (Foreign << DiagnosesMsg) <| Diagnoses.view dm



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
            case Route.fromUrl url of
                Just route ->
                    case route of
                        Route.Home ->
                            start Home HomeMsg (Home.init ()) model

                        Route.Cats ->
                            start Cats CatsMsg (Cats.init ()) model

                        Route.Diagnoses ->
                            start Diagnoses DiagnosesMsg (Diagnoses.init ()) model

                Nothing ->
                    ( model, Cmd.none )

        Foreign fmsg ->
            case ( fmsg, model.page ) of
                ( HomeMsg hmsg, Home hm ) ->
                    handleForeignMsg Home HomeMsg Home.update hmsg hm model

                ( CatsMsg cmsg, Cats cm ) ->
                    handleForeignMsg Cats CatsMsg Cats.update cmsg cm model

                ( DiagnosesMsg dmsg, Diagnoses dm ) ->
                    handleForeignMsg Diagnoses DiagnosesMsg Diagnoses.update dmsg dm model

                -- Cringe Elm moment
                _ ->
                    ( model, Cmd.none )


handleForeignMsg :
    (model -> Page)
    -> (msg -> Foreign)
    -> (msg -> model -> ( model, Cmd msg ))
    -> msg
    -> model
    -> Model
    -> ( Model, Cmd Msg )
handleForeignMsg page msg updateFn fmsg fmodel model =
    start page msg (updateFn fmsg fmodel) model


start : (model -> Page) -> (msg -> Foreign) -> ( model, Cmd msg ) -> Model -> ( Model, Cmd Msg )
start page msg ( newModel, cmd ) model =
    ( { model
      | page = page newModel
      }
    , Cmd.map (Foreign << msg) cmd
    )


subscriptions : Model -> Sub msg
subscriptions =
    always Sub.none


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
