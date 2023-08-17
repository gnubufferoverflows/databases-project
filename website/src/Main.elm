module Main exposing (main)


import Browser
import Html exposing (Html)
import Html.Attributes exposing (class, src)
import Http
import Json.Decode as Decode exposing (Decoder)
import Process
import Task
import Url.Builder


type alias CatCard =
    { id : Int
    , imageUrl : String
    }


type Model
    = Loading
    | CatCards (List CatCard)
    | TimeoutError Int
    | IrrecoverableError


type Msg
    = GotCatCards (Result Http.Error (List CatCard))
    | TryAgain (Cmd Msg) ()


init : () -> ( Model, Cmd Msg )
init =
    always ( Loading, requestCatCards )


requestCatCards : Cmd Msg
requestCatCards = 
    Http.get
        { url = Url.Builder.absolute ["api", "cats"] []
        , expect = Http.expectJson GotCatCards <| Decode.list decodeCatCard
        }


decodeCatCard : Decoder CatCard
decodeCatCard =
    Decode.map2 CatCard
        (Decode.field "id" Decode.int)
        (Decode.field "imageUrl" Decode.string)


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            Html.p [] [Html.text "Loading..."]

        CatCards cats ->
            cats |> List.map catCard >> Html.div [class "tile", class "is-ancestor"]

        TimeoutError _ ->
            Html.p [] [Html.text <| "Timed out; retry in " ++ String.fromInt (truncate <| timeoutWait / 1000) ++ " seconds."]

        IrrecoverableError ->
            Html.p [] [Html.text "An irrecoverable error has occurred. Maybe try again (manually; refresh the page)?"]


{-
Given input of imageUrl, this function returns the following HTML:

<div class="tile is-parent">
    <article class="tile is-child box">
        <p class="title">Haskell</p>
        <figure class="image">
            <img src=imageUrl>
        </figure>
        <a href="cat_record.html">Learn more</a>
    </article>
</div>
-}
catCard : CatCard -> Html Msg
catCard {id, imageUrl} =
    Html.div [class "tile", class "is-parent"]
        [ Html.article [class "tile", class "is-child", class "box"]
            [ Html.p [class "title"] [Html.text "Haskell"] 
            , Html.figure [class "image"] [Html.img [src imageUrl] []]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCatCards result ->
            handleResult CatCards requestCatCards result model
            
        TryAgain cmd _ ->
            ( model, cmd )


timeoutLimit : Int
timeoutLimit =
    3


type alias Milliseconds =
    Float


timeoutWait : Milliseconds
timeoutWait =
    3000


handleResult : (a -> Model) -> Cmd Msg -> Result Http.Error a -> Model -> ( Model, Cmd Msg )
handleResult interp response result model =
    case result of
        Ok good ->
            ( interp good, Cmd.none )

        Err Http.Timeout ->
            case model of
                TimeoutError x ->
                    if x < timeoutLimit then
                        ( TimeoutError <| x + 1, tryAgain response )
                    else
                        ( IrrecoverableError, Cmd.none )

                _ ->
                    ( TimeoutError 0, tryAgain response )

        _ ->
            ( IrrecoverableError, Cmd.none )


tryAgain : Cmd Msg -> Cmd Msg
tryAgain cmd =
    Task.perform (TryAgain cmd) <| Process.sleep timeoutWait


subscriptions : Model -> Sub msg
subscriptions =
    always Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
