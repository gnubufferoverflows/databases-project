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
    = Text TextType String
    | DropDown (Dict Int String) (Maybe Int)


type TextType
    = Date
    | TextArea
    | Input (String -> Maybe String)


type Msg a
    -- Create
    = Create a
    | BeginCreate
    | Created a (Result Http.Error Int)
    -- Read
    | GotData (Result Http.Error (Dict Int a))
    | GotSubquery String (Result Http.Error (Dict Int String))
    -- Update
    | Update Int a
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
    = Projection (a -> String) (Maybe ( String, TextType )) -- a function from the object to a String representation of some aspect of it
    | Subquery String (a -> Int) (Maybe String) -- a subquery key that will be looked up in the subquery dictionary


type WritableCellView a
    = WProjection (a -> String) TextType
    | WSubquery String (a -> Int)


type alias WritableColumn a =
    ( String, WritableCellView a )


-- unzipped dataview, for efficient access of necessary subcomponents
-- this ensures that every list in the dataview is the same length,
-- impossible to enforce otherwise
type alias UnzippedColumns a =
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


type alias InitStuff a =
    { read : Endpoint
    , subqueries : Dict String Endpoint
    , decoder : Decoder a
    }


-- initial state and loading side-effect
init : InitStuff a -> () -> ( Model a, Cmd (Msg a) )
init initstuff =
    always
        ( Dict.size initstuff.subqueries |> unloaded |> PartiallyLoaded
        , fetchAllData initstuff
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
fetchAllData : InitStuff a -> Cmd (Msg a)
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
update : CrudEndpoints -> (a -> Value) -> UnzippedColumns a -> Msg a -> Model a -> ( Model a, Cmd (Msg a) )
update endpoints encoder columns msg model =
    case ( msg, model ) of
        -- CREATE

        ( Create value, Loaded state ) ->
            ( Loaded {state | modifying = Nothing}, create endpoints.create encoder value )

        ( BeginCreate, Loaded ({subqueries} as state) ) ->
            ( {state | modifying = Just <| Creating <| makeCreateInterface subqueries columns}, Cmd.none )

        ( Created value id, Loaded state ) ->
            id |> insertNewValue value state

        -- READ

        -- we got the main data
        ( GotData result, PartiallyLoaded partialData ) ->
            result |> loadData partialData

        -- we got the result from a subquery
        ( GotSubquery key result, PartiallyLoaded partialData ) ->
            result |> loadSubquery partialData key

        -- UPDATE

        ( Update id value, Loaded state ) ->
            ( Loaded <| updateValue id value state, update endpoints.update encoder id value )

        -- we are beginning an edit
        ( BeginEdit id object, Loaded ({subqueries} as state) ) ->
            ( {state | modifying = Just <| Editing id <| makeEditInterface subqueries object columns}, Cmd.none )

        -- DELETE

        ( Delete id, Loaded ({data} as state) ) ->
            ( Loaded
                { state
                | modifying = Nothing
                , data = Dict.remove id data
                }
            , delete endpoints.delete id
            )

        ( BeginDelete id, Loaded state ) ->
            ( {state | modifying = Just <| Deleting id, Cmd.none )

        -- Modal operations

        ( UpdateField key newValue, Loading state ) ->
            updateField key newValue state

        ( CloseModal, Loaded state ) ->
            ( {state | modifying = Nothing}, Cmd.none )

        -- Elm moment
        -- I don't specifically have a case for Acked, because I don't think it's very important
        _ ->
            ( model, Cmd.none )


delete : Endpoint -> Int -> Cmd (Msg a)
delete endpoint id =
    Http.post
        { url = endpoint
        , body = Http.jsonBody <| Enc.int id
        , expect = Http.expectWhatever Acked
        }


updateField : String -> InputType -> State a -> ( Model a, Cmd (Msg a) )
updateField key newValue ({modifying} as state) =
    ( { state
      | modifying =
            case modifying of
                Editing id interface ->
                    Editing id <| Dict.insert key newValue interface

                Creating interface ->
                    Creating <| Dict.insert key newValue interface

                _ ->
                    modifying
     }
    , Cmd.none
    )


updateValue : Int -> a -> State a -> State a
updateValue id value ({data} as state) =
    { state
    | modifying = Nothing
    , data = Dict.insert id value data
    }


update : Endpoint -> (a -> Value) -> Int -> a -> Cmd (Msg a)
update endpoint encode id value =
    Http.post
        { url = endpoint
        , body = Http.jsonBody <| Enc.object [ ( "id", Enc.int id ), ( "value", encode value ) ]
        , expect = Http.expectWhatever Acked
        }


create : Endpoint -> (a -> Value) -> a -> Cmd (Msg a)
create endpoint encode value = 
    Http.post
        { url = url
        , body = Http.jsonBody <| encode value
        , expect = Http.expectJson (Created value) Dec.int
        }


insertNewValue : a -> State a -> Result Http.Error Int -> ( Model a, Cmd (Msg a) )
insertNewValue value ({data} as state) =
    handleRequest
        <| \ id -> Loaded {state | data = Dict.insert id value data}


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
            Text tt ""

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
            Text tt <| f object

        WSubquery key f ->
            Just (f object)
                |> DropDown (Dict.get key subqueries)


filterMapToWritable : List (Column a) -> List (WritableColumn a)
filterMapToWritable =
    List.filterMap writableColumn


writableColumn : Column a -> Maybe (WritableColumn a)
writableColumn column =
    case column.view of
        Projection f (Just ( key, tt )) ->
            Just ( key, WProjection f tt)

        Subquery key f (Just ikey) ->
           Just ( ikey, WSubquery key f )

        _ ->
           Nothing 
            

loadData : PartialModel a -> Result Http.Error (Dict Int a) -> ( Model a, Cmd (Msg a) )
loadData ({subqueries, subqueriesToFulfill} as partialData) key =
    handleRequest
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


loadSubquery : PartialModel a -> String -> Result Http.Error (Dict Int String) -> ( Model a, Cmd (Msg a) )
loadSubquery ({subqueries, data, subqueriesToFulfill} as partialData) key =
    handleRequest
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


-- function to handle http errors from requests in this module in a concise way
handleRequest : (x -> Model a) -> Result Http.Error x -> ( Model a, Cmd (Msg a) )
handleRequest handler result =
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
                , modifying
                    |> Maybe.map (makeModal decoder headers)
                    |> Maybe.withDefault emptyHtml
                ]


makeModal : Decoder a -> List String -> Modifying -> Html (Msg a)
makeModal decoder headers m =
    case m of
        Editing id interface ->
            makeEditModal decoder headers id interface
        
        Creating interface ->
            makeCreateModal decoder headers interface

        Deleting id ->
            makeDeleteModal id


makeEditModal : Decoder a -> List String -> Int -> Interface -> Html (Msg a)
makeEditModal decoder headers interface =
    modal
        { header = "Editing"
        , footer = "Submit"
        , onSubmit = nuclearWaste decoder interface <| Update id
        , body = interfaceBody headers interface
        }


makeCreateModal : Decoder a -> List String -> Interface -> Html (Msg a)
makeCreateModal decoder headers interface =
    modal
        { header = "Creating"
        , footer = "Submit"
        , onSubmit = nuclearWaste decoder interface Create
        , body = interfaceBody headers interface
        }


makeDeleteModal : Int -> Html (Msg a)
makeDeleteModal id =
    modal
        { header = "Deleting"
        , footer = "Confirm"
        , onSubmit = Just <| Delete id
        , body = [Html.h1 [class "title"] [Html.text "Are you sure?"]]
        }


-- runs the decoder on the interface
nuclearWaste : Decoder a -> Interface -> (a -> Msg a) -> Maybe (Msg a)
nuclearWaste decoder interface msg =
    Enc.dict identity encodeInput interface
        |> Dec.decodeValue decoder
        |> Result.toMaybe
        |> Maybe.map msg


encodeInput : InputType -> Value
encodeInput inputType =
    case inputType of
        Text _ value ->
            Enc.string value

        DropDown _ m ->
            Maybe.map Enc.int m
                |> Maybe.withDefault Enc.null


interfaceBody : List String -> Interface -> List (Html (Msg a))
interfaceBody headers =
    Dict.toList
        >> List.map2 formField headers


formField : String -> ( String, InputType ) -> Html (Msg a)
formField label ( key, contents ) =
    Html.div [class "field"]
        [ Html.label [class "field-label"] [Html.text label]
        , Html.div [class "field-body"]
            [formInput key contents]
        ]


formInput : String -> InputType -> Html (Msg a)
formInput key contents =
    case contents of
        DropDown domain value ->
            makeDropdown key domain value


makeDropdown : String -> Dict Int String -> Maybe Int -> Html (Msg a)
makeDropdown key domain value =
    Html.div [class "dropdown"]
        [ Html.div [class "dropdown-trigger"]
            [ Html.button [class "button", ariaHasPopup, ariaControls "dropdown-menu"]
                [ Html.span []
                    [ Html.text
                        ( value
                            |> Maybe.andThen ( \ k -> Dict.get k domain )
                            |> Maybe.withDefault "Select..."
                        )
                    ]
                , Html.span [class "icon", class "is-small"]
                    [Html.i [class "fas", class "fa-angle-down", ariaHidden]
                ]
            ]
        , Html.div [class "dropdown-menu", id "dropdown-menu", role "menu"]
            [ Html.div [class "dropdown-content"]
                <| populateDropdown key domain
            ]
        ]


populateDropdown : String -> Dict Int String -> List (Html (Msg a))
populateDropdown ikey menu =
    Dict.toList menu
        |> List.map
            ( \ ( id, x ) ->
                Html.a
                    [ class "dropdown-item"
                    , Events.onClick <| UpdateField ikey <| DropDown menu <| Just id 
                    ]
                    [Html.text x]
            )
                

updateTextInterface : String -> Text -> String -> Msg a
updateTextInterface key textType =
    Text textType
        >> UpdateField key


emptyHtml : Html msg
emptyHtml =
    Html.text ""


isNothing : Maybe a -> Bool
isNothing m =
    case m of
        Nothing ->
            True

        _ ->
            False


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
    , onSubmit : Maybe (Msg a)
    }


modal : ModalOptions a -> Html (Msg a)
modal options =
    Html.div
        [ class "modal"
        , class "is-active"    
        ]
        [ Html.div [class "modal-background", Events.onClick CloseModal] []
        , Html.div [class "modal-card"]
            [ Html.header [class "modal-card-head"]
                [ Html.p [class "modal-card-title"] [Html.text options.header]
                , Html.button [class "delete", ariaLabel "close", Events.onClick CloseModal] []
                ]
            , Html.section [class "modal-card-body"] options.body
            , Html.footer [class "modal-card-foot"]
                [ Html.div [class "field", class "is-grouped"]
                    [ Html.p [class "control"]
                        -- this is something Elm should address
                        [ Html.a (List.filterMap identity [Maybe.map Events.onClick options.onSubmit, Just <| class "button", Just <| class "is-danger"])
                            [Html.text options.footer]
                        ]
                    ]
                ]
            ]
        ]


role : String -> Attribute Msg
role =
    Html.Attributes.attribute "role"


ariaLabel : String -> Attribute msg
ariaLabel =
    Html.Attributes.attribute "aria-label"


ariaHidden : Attribute msg
ariaHidden =
    Html.Attributes.attribute "aria-hidden" "true"


ariaHasPopup : Attribute msg
ariaHasPopup =
    Html.Attributes.attribute "aria-haspopup" "true"


ariaControls : String -> Attribute msg
ariaControls =
    Html.Attributes.attribute "aria-controls"


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
    , encoder : a -> Value
    }
    -> Program () (Model a) (Msg a)
main {read, create, edit, delete, columns, encoder, decoder} =
    -- TODO: Need to finish this
