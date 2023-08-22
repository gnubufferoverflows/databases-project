-- the main page, that displays all of the funny cat cards


module Pages.Home exposing
    ( Flags
    , Model
    , Msg
    , init
    , view
    , update
    , subscriptions
    )


import Browser
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events
import Http
import Json.Decode exposing (Decoder)
import Url.Builder


import EESE
import EESE.Html
import EESE.Http
import EESE.Json.Decode


type alias Flags =
    ()


type Model
    = Loading
    | Error String
    | CatCards (List CatCard)


type alias CatCard =
    { id : Int
    , imageUrl : String
    }


type Msg
    = GotCatCards (Result Http.Error (List CatCard))


init : Flags -> ( Model, Cmd Msg )
init =
    always ( Loading, loadCatCards )


catCardEndpoint : String
catCardEndpoint =
    Url.Builder.absolute ["api", "cats"] []


loadCatCards : Cmd Msg
loadCatCards =
    Http.get
        { url = catCardEndpoint
        , expect = Http.expectJson GotCatCards <| Json.Decode.list decodeCatCard
        }


decodeCatCard : Decoder CatCard
decodeCatCard =
    Json.Decode.map (EESE.uncurry CatCard)
        <| EESE.Json.Decode.tuple
            Json.Decode.int <|
            Json.Decode.field "imageURL" Json.Decode.string


view : Model -> Browser.Document Msg
view model =
    { title = "Adopt A Haskell"
    , body = viewBody model
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    case model of
        Loading ->
            [EESE.Html.empty]

        Error msg ->
            [Html.p [] [Html.text msg]]
            
        CatCards cards ->
            adoptAllHaskellsButton
                :: 
                    (
                        cards
                            |> List.map viewCatCard
                    )


adoptAllHaskellsButton : Html Msg
adoptAllHaskellsButton =
    Html.a
        [ class "button"
        , Html.Attributes.href <| Url.Builder.absolute ["adopt", "all"] []
        ]
        [Html.text "Adopt All Haskells"]


viewCatCard : CatCard -> Html Msg
viewCatCard card =
    Html.div
        [ class "tile"
        , class "is-ancestor"
        ]
        [ Html.div [class "tile", class "is-parent"]
            [ Html.article [class "tile", class "is-child", class "box"]
                [ Html.p [class "title"] [Html.text "Haskell"]
                , Html.figure [class "image"]
                    [Html.img [Html.Attributes.src card.imageUrl] []]
                , Html.a [Html.Attributes.href <| Url.Builder.absolute ["cats", String.fromInt card.id] []]
                    [Html.text "Learn more"]
                ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCatCards result ->
            case result of
                Err e ->
                    ( Error <| EESE.Http.httpErrorToString e, Cmd.none )

                Ok cats ->
                    ( CatCards cats, Cmd.none )


subscriptions : Model -> Sub msg
subscriptions =
    always Sub.none
