module Crud exposing (Model(..), Msg(..), main, Column, Endpoint, CellView(..))


import Browser
import Html exposing (Html)
import Html.Attributes as Attrs exposing (class, id, classList, Attribute)
import Html.Events
import Http
import Json.Decode as Dec exposing (Decoder)
import Json.Encode as Enc exposing (Value)


-- type to represent the state of the crud app
-- Crud is either loading, in error state, or running
type Model a
    = PartiallyLoaded (PartialModel a)
    | Error String
    | Running (State a)


type alias PartialModel a =
    { subqueries : Subqueries
    , data : Maybe (Dict Int a)
    , subqueriesToFulfill : Int
    }


type alias Subqueries =
    Dict String (Dict Int String)


type alias State a =
    { subqueries : Subqueries -- a list of string-keyed subquery results for enumerated table cells
    , data : Dict Int a -- the collection of entities operated upon
    , modifying : Maybe Modifying -- the state of editing the entities
    }


-- type to represent the state of modifying the page
-- either we are not modifying anything, we are editing a particular entity,
-- or we are creating a new entity
type Modifying
    = Editing Int Interface
    | Creating Interface
    | Deleting Int


-- internal storage for user interface for creating and editing entities
type alias Interface =
    Dict String InputType


type InputType
    = Text Text
    | DropDown (Dict Int String) (Maybe Int)


type alias Text =
    { textType : TextType
    , value : String
    }


type TextType
    = Date
    | TextArea
    | Input (String -> Maybe String)


type Msg a
    -- Create
    = Create Value
    | BeginCreate
    -- Read
    | GotData (Result Http.Error (Dict Int a))
    | GotSubquery String (Result Http.Error (Dict Int String))
    -- Update
    | Update Int Value
    | BeginEdit Int a
    -- Delete
    | Delete Int
    | BeginDelete Int
    -- Modal operations
    | UpdateField String InputType
    | CloseModal
    -- Server communication
    | Acked (Result Http.Error ())
    

type alias Endpoint =
    String


type alias Column a =
    { header : String -- the name of the column
    , view : CellView a -- the kind of view of the object that the column will provide
    }


type CellView a
    = Projection (a -> String) (Maybe TextType) -- a function from the object to a String representation of some aspect of it
    | Subquery String (a -> Int) Bool -- a subquery key that will be looked up in the subquery dictionary


-- unzipped dataview, for efficient access of necessary subcomponents
-- this ensures that every list in the dataview is the same length,
-- impossible to enforce otherwise
type alias UnzippedColumns a
    { headers : List String
    , views : List (CellView a)
    }


-- unzips a list of columns into a record with multiple lists for easier processing
unzipColumns : List (Column a) -> UnzippedColumns a
unzipColumns =
    List.foldr dataViewCons emptyDataView


zipColumns : UnzippedColumns a -> List (Column a)
zipColumns {headers, views} =
    List.map2 Column headers views


columnCons : Column a -> UnzippedColumns a -> UnzippedColumns a
columnCons {header, view} {headers, views} =
    {headers = header :: headers, views = view :: views}


emptyUnzippedColumns : UnzippedColumns a
emptyUnzippedColumns =
    {headers = [], views = []}


type alias ReadEndpoints a =
    { read : Endpoint
    , subqueries : Dict String Endpoint
    , decoder : Decoder a
    }


-- initial state and loading side-effect
init : ReadEndpoints a -> () -> ( Model a, Cmd (Msg a) )
init endpoints =
    always
        ( Dict.size endpoints.subqueries |> unloaded |> PartiallyLoaded
        , fetchAllData endpoints
        )


-- the unloaded state
unloaded : Int -> PartialModel a
unloaded size =
    { subqueries = Dict.empty
    , data = Nothing
    , subqueriesToFulfill = size
    }


-- fetch the main data and all subqueries
-- performs this task concurrently
fetchAllData : ReadEndpoints a -> Cmd (Msg a)
fetchAllData {read, subqueries, decoder} =
    Cmd.batch
        <| fetchMainData read decoder :: fetchSubqueries subqueries


-- gets the main data, the stuff that will populate the main table
fetchMainData : Endpoint -> Decoder a -> Cmd (Msg a)
fetchMainData endpoint decoder =
    Http.get
        { url = endpoint
        , expect = Http.expectJson GotData <| decodeIntDict decoder
        }


-- spins up all of the subqueries into a list of commands to be batched
fetchSubqueries : Dict String Endpoint -> List (Cmd (Msg a))
fetchSubqueries =
    Dict.toList
        >> List.map
            ( \ ( key, endpoint ) ->
                Html.get
                    { url = endpoint
                    , expect = Http.expectJson (GotSubquery key) <| decodeIntDict Dec.string
                    }
            )


-- augments a decoder to be over the values of a dict from int to a
decodeIntDict : Decoder a -> Decoder (Dict Int a)
decodeIntDict decoder =
    Dec.map (mapFilterKeys String.toInt)
        <| Dec.dict decoder


-- maps over the keys of a dictionary, filtering out keys that do not parse
mapFilterKeys : (k -> Maybe n) -> Dict k v -> Dict n v
mapFilterKeys f =
    Dict.empty
        |> Dict.foldl
            ( \ k v ->
                case f k of
                    Just n ->
                        Dict.insert n v

                    _ ->
                        identity
            )


type alias CrudEndpoints =
    { create : Endpoint
    , edit : Endpoint
    , update : Endpoint
    , delete : Endpoint
    }


-- main update loop
-- what will we do when we get a new message?
update :
    { endpoints : CrudEndpoints
    , columns : UnzippedColumns a
    }
    -> Msg a
    -> Model a
    -> ( Model a, Cmd (Msg a) )
update {endpoints, columns} msg model =
    case ( msg, model ) of
        -- CREATE

        ( BeginCreate, Loaded ({subqueries} as state) ) ->
            ( {state | modifying = Just <| Creating <| makeCreateInterface subqueries columns}, Cmd.none )

        -- READ

        -- we got the main data
        ( GotData result, PartiallyLoaded partialData ) ->
            result |> loadData partialData

        -- we got the result from a subquery
        ( GotSubquery key result, PartiallyLoaded partialData ) ->
            result |> loadSubquery partialData key

        -- UPDATE

        -- we are beginning an edit
        ( BeginEdit id object, Loaded ({subqueries} as state) ) ->
            ( {state | modifying = Just <| Editing id <| makeEditInterface subqueries object columns}, Cmd.none)

        -- DELETE

        -- Modal operations
        ( CloseModal, Loaded state ) ->
            ( {state | modifying = Nothing}, Cmd.none )

        -- Elm moment
        _ ->
            ( model, Cmd.none )


type alias WritableColumn a =
    ( String, WritableCellView a )


type WritableCellView a
    = WProjection (a -> String) TextType
    | WSubquery String (a -> Int)


makeCreateInterface : Subqueries -> UnzippedColumns a -> Interface
makeCreateInterface subqueries =
    zipColumns
        >> filterMapToWritable
        >> List.map (Tuple.mapSecond <| toEmptyInput subqueries)
        >> Dict.fromList


toEmptyInput : Subqueries -> WritableCellView a -> InputType
toEmptyInput subqueries w =
    case w of
        WProjection _ tt ->
            Text
                { textType = tt
                , value = ""
                }

        WSubquery key _ ->
            DropDown (Dict.get key subqueries |> Maybe.withDefault Dict.empty) Nothing


makeEditInterface : Subqueries -> a -> UnzippedColumns a -> Interface
makeEditInterface subqueries object =
    zipColumns
        >> filterMapToWritable
        >> List.map (Tuple.mapSecond <| populateEditInput subqueries object)
        >> Dict.fromList


populateEditInput : Subqueries -> a -> WritableCellView a -> InputType
populateEditInput subqueries object w =
    case w of
        WProjection f tt ->
            Text
                { textType = tt
                , value = f object
                }

        WSubquery key f ->
            Just (f object)
                |> DropDown (Dict.get key subqueries)


filterMapToWritable : List (Column a) -> List (WritableColumn a)
filterMapToWritable =
    List.filterMap writableColumn


writableColumn : Column a -> Maybe (WritableColumn a)
writableColumn column =
    case column.view of
        Projection f (Just tt) ->
            Just ( column.header, WProjection f tt )

        Subquery key f True ->
           Just ( column.header, WSubquery key f )

        _ ->
           Nothing 
            

loadData : PartialModel a -> Result Http.Error x -> ( Model a, Cmd (Msg a) )
loadData ({subqueries, subqueriesToFulfill} as partialData) key =
    handleGetRequest
        <| \ xs ->
            case subqueriesToFulfill of
                0 ->
                    Running
                        { subqueries = subqueries
                        , data = xs
                        , modifying = Nothing
                        }

                _ ->
                    PartiallyLoaded
                        {partialData | data = Just xs}


loadSubquery : PartialModel a -> String -> Result Http.Error x -> ( Model a, Cmd (Msg a) )
loadSubquery ({subqueries, data, subqueriesToFulfill} as partialData) key =
    handleGetRequest
        <| \ xs ->
            case ( data, subqueriesToFulfill ) of
                ( Just ys, 1 ) ->
                    Running
                        { subqueries = Dict.insert key xs subqueries
                        , data = ys
                        , modifying = Nothing
                        }

                    _ ->
                        PartiallyLoaded
                            { partialData
                            | subqueries = Dict.insert key xs subqueries
                            , subqueriesToFulfill = subqueriesToFulfill - 1
                            }


-- function to handle http errors from get requests in this module in a concise way
handleGetRequest :
    (x -> Model a)
    -> Result Http.Error x -- the result to handle
    -> ( Model a, Cmd (Msg a) )
handleGetRequest handler result =
    ( case result of
        Err e ->
            Error <| httpErrorToString e

        Ok x ->
            handler x

    , Cmd.none
    )


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        BadUrl x ->
            "The url at " ++ x ++ " could not be located."

        Timeout ->
            "Timed out."

        NetworkError ->
            "A network error occurred with your connection."

        BadStatus x ->
            "Bad status: " ++ String.fromInt x ++ "."

        BadBody x ->
            "Bad body: " ++ x ++ "."


-- function to determine what we will render
view : Decoder a -> UnzippedColumns a -> Model a -> Html (Msg a)
view decoder {headers, views} model =
    case model of
        PartiallyLoaded _ ->
            Html.p [] [Html.text "Loading"]

        Error msg ->
            Html.p [] [Html.text msg]

        Running {subqueries, data, modifying} ->
            Html.div []
                [ creationButton
                , makeTable headers views subqueries data
                , Maybe.map makeModal modifying
                    |> Maybe.withDefault emptyHtml
                ]


makeModal : Decoder a -> Modifying -> Html (Msg a)
makeModal m =
    case m of
        Editing id interface ->
            makeEditModal id interface
        
        Creating interface ->
            makeCreateModal interface

        Deleting id ->
            makeDeleteModal id


-- decoder may or may not be necessary
makeEditModal : Decoder a -> Int -> Interface -> Html (Msg a)
makeEditModal id interface =
    modal
        { header = "Editing"
        , footer = "Submit"
        , onSubmit = nuclearWaste interface <| Update id
        , body = interfaceBody interface
        }


interfaceBody : Interface -> List (Html (Msg a))
interfaceBody =
    Dict.toList 
        >> List.map
            ( \ ( label, contents ) ->
                Html.div [class "field"]
                    [ Html.label [class "field-label"] [Html.text label]
                    , Html.div [class "field-body"]
                        [ case contents of
                            Text {textType = Date, value} ->
                                Html.input
                                    [ Attrs.type_ "date"
                                    , Attrs.value value
                                    , Events.onInput <| UpdateField label -- needs to be modified
                                    ]
                                    []

                            Text {textType = TextArea, value} ->
                                Html.textArea
                                    [ Attrs.value value
                                    , Events.onInput <| UpdateField label
                                    ]


emptyHtml : Html msg
emptyHtml =
    Html.text ""


creationButton : Html (Msg a)
creationButton =
    Html.button
        [ class "button"
        , class "is-primary"
        , class "js-modal-trigger"
        , Events.onClick BeginCreate
        ]
        []


-- creates the main data table
makeTable : List String -> List (a -> String) -> Subqueries -> Dict Int a -> Html (Msg a)
makeTable headers views subqueries data =
    Html.table [class "table"]
        [ Html.thead []
            <| List.map (wrapElement <| Html.th []) headers
                ++ [Html.th [] [Html.text "Action"]]
        , Html.tbody []
            <| List.map (toRow views subqueries) (Dict.toList data)
        ]


-- converts a particular element into a row in the main data table
toRow : List (CellView a) -> Subqueries -> ( Int, a ) -> List (Html (Msg a))
toRow views subqueries ( id, object ) =
    Html.tr []
        <| List.map (wrapElement <| Html.td []) (projectWholeObject subqueries object views)
            ++ [ Html.td []
                    [ Html.a [class "js-modal-trigger", Events.onClick <| BeginEdit id object]
                        [Html.text "Edit"]
                    , Html.br [] []
                    , Html.a [class "has-text-danger", class "js-modal-trigger"]
                        [Html.text "Delete"]
                    ]
                ]


projectWholeObject : Subqueries -> a -> List (CellView a) -> List String
projectWholeObject subqueries object =
    List.map <| projectObject subqueries object


projectObject : Subqueries -> a -> CellView a -> String
projectObject subqueries object cellview =
    case cellView of
        Projection f _ ->
            f object

        Subquery key f _ ->
            Dict.get key subqueries
                |> Maybe.andThen (Dict.get <| f object)
                |> Maybe.withDefault ""



wrapElement : (List (Html msg) -> Html msg) -> String -> Html msg
wrapElement element =
    Html.text >> List.singleton >> element


type alias ModalOptions a =
    { header : String
    , body : List (Html (Msg a))
    , footer : String
    , onSubmit : Msg a
    }


modal : ModalOptions a ->  Html (Msg a)
modal options =
    Html.div
        [ class "modal"
        , class "is-active"    
        ]
        [ Html.div [class "modal-background", Events.onClick CloseModal] []
        , Html.div [class "modal-card"]
            [ Html.header [class "modal-card-head"]
                [ Html.p [class "modal-card-title"] [Html.text options.header]
                , Html.button [class "delete", ariaLabel "close", Events.onClick options.onClose] []
                ]
            , Html.section [class "modal-card-body"] body
            , Html.footer [class "modal-card-foot"]
                [ Html.div [class "field", class "is-grouped"]
                    [ Html.p [class "control"]
                        [ Html.a [class "button", class "is-danger", Events.onClick options.onSubmit]
                            [Html.text options.footer]
                        ]
                    ]
                ]
            ]
        ]


ariaLabel : String -> Attribute msg
ariaLabel =
    Html.Attributes.attribute "aria-label"


subscriptions : Model a -> Sub msg
subscriptions =
    always Sub.none


main :
    { read : Endpoint
    , create : Endpoint
    , edit : Endpoint
    , delete : Endpoint
    , subqueries : Dict String Endpoint
    , columns : List (Column a)
    , decoder : Decoder a
    }
    -> Program () (Model a) (Msg a)
main {read, create, edit, delete, columns, decoder} =
