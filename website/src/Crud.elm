-- Crud.
-- By Evan Hock.
--
-- Generalized CRUD API for building trash-tier government webpages that essentially
-- display a big old table and have some rudimentary options for tweaking said data.
--
-- It works by defining a handful of endpoints, functions, and decoders for each page that you want to make, and then calling `Crud.main` with
-- the special object that allows you to package all of those functions into the API.
--
-- That object is as follows:
--
-- type alias Endpoint = String
--
-- type alias Column a =
-- { header : String -- the name of the column
-- , view : CellView a -- the view of the "topic data type" that the particular column represents
-- }
-- 
-- { read : Endpoint -- the read endpoint
-- , create : Endpoint -- the create endpoint
-- , update : Endpoint -- the update/edit endpoint
-- , delete : Endpoint -- the delete endpoint
-- , subqueries : Dict String Endpoint -- a dict of string keys to subquery endpoints, for the subqueries
-- , columns : List (Column a) -- a list of column objects
-- , decoder : Json.Decode.Decoder a -- a decoder for the "topic object"
-- , encoder : a -> Json.Encode.Value -- an encoder for the "topic object"
-- }
--
-- The concept of the "topic object" is very important for this module. Essentially, all of these
-- low-quality CRUD pages one can make with this module must revolve around one particular datatype.

module Crud exposing (Model(..), Msg(..), main, Column, Endpoint, CellView(..))


import Browser
import Dict exposing (Dict)
import EESE.Html as EvanHtml
import EESE.Html.Aria as Aria
import EESE.Http as EvanHttp
import EESE.Json.Decode as EvanJsonDecoders
import Html exposing (Html)
import Html.Attributes as Attrs exposing (class, id, classList)
import Html.Events as Events
import Http
import Json.Decode as Dec exposing (Decoder)
import Json.Encode as Enc exposing (Value)


-- type to represent the state of the crud app
-- Crud is either loading, in error state, or running
type Model a
    = PartiallyLoaded (PartialModel a)
    | Error String
    | Loaded (State a)


-- represents the partially loaded model while data is still coming in
type alias PartialModel a =
    { subqueries : Subqueries
    , data : Maybe (Dict Int a)
    , subqueriesToFulfill : Int
    }


-- this type comes up a lot whenever subqueries are involved, so I aliased it
type alias Subqueries =
    Dict String (Dict Int String)


-- the state of the webpage, once everything is up and running
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


-- used for user input, types input types as either text or dropdown
type InputType
    = Text TextType String
    | DropDown (Dict Int String) (Maybe Int)


-- used for user input, further differentiates text inputs (inputs that use the input or textarea tags)
type TextType
    = Date
    | TextArea
    | Input (String -> Bool)


-- the elm runtime system revolves around the concept of a "message-passing" programming style,
-- and the message types represent these particular messages. i've roughly divided them into category.
-- their names are mostly self-explanatory
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
    

-- endpoint type alias
type alias Endpoint =
    String


-- the column type. represents a column in the topic table
type alias Column a =
    { header : String -- the name of the column
    , view : CellView a -- the kind of view of the object that the column will provide
    }


-- cellview represents how to display the data associated with a particular column
type CellView a
    = Projection (a -> String) (Maybe ( String, TextType )) -- a function from the object to a String representation of some aspect of it
    | Subquery String (a -> Int) (Maybe String) -- a subquery key that will be looked up in the subquery dictionary


-- a type for storing the writable subset of cellview in a more convenient manner
type WritableCellView a
    = WProjection (a -> String) TextType
    | WSubquery String (a -> Int)


-- a simple tuple type that rather than a real type. not exported
type alias WritableColumn a =
    ( String, WritableCellView a )


-- unzipped dataview, for efficient access of necessary subcomponents
-- this ensures that every list in the dataview is the same length,
-- impossible to enforce otherwise
type alias UnzippedColumns a =
    { headers : List String
    , views : List (CellView a)
    }


-- unzips a list of columns into a record with multiple lists for easier processing. the inverse of zipColumns
unzipColumns : List (Column a) -> UnzippedColumns a
unzipColumns =
    List.foldr columnCons emptyUnzippedColumns


-- the inverse of unzipColumns
zipColumns : UnzippedColumns a -> List (Column a)
zipColumns {headers, views} =
    List.map2 Column headers views


-- given a column and an UnzippedColumns object, append the elements of that column to each of the UnzippedColumns object's element lists
columnCons : Column a -> UnzippedColumns a -> UnzippedColumns a
columnCons column {headers, views} =
    {headers = column.header :: headers, views = column.view :: views}


-- represents the empty UnzippedColumns object
emptyUnzippedColumns : UnzippedColumns a
emptyUnzippedColumns =
    {headers = [], views = []}


-- INIT


-- a type alias for the list of configuration data passed to init, as this type of list pops up more than once
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
        , expect = Http.expectJson GotData <| EvanJsonDecoders.intDict decoder
        }


-- spins up all of the subqueries into a list of commands to be batched
fetchSubqueries : Dict String Endpoint -> List (Cmd (Msg a))
fetchSubqueries =
    Dict.toList
        >> List.map
            ( \ ( key, endpoint ) ->
                Http.get
                    { url = endpoint
                    , expect = Http.expectJson (GotSubquery key) <| EvanJsonDecoders.intDict Dec.string
                    }
            )


-- UPDATE


-- type for crud endpoints that the `update` function uses
type alias CrudEndpoints =
    { create : Endpoint
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
            ( Loaded {state | modifying = Nothing}, createCmd endpoints.create encoder value )

        ( BeginCreate, Loaded ({subqueries} as state) ) ->
            ( Loaded {state | modifying = Just <| Creating <| makeCreateInterface subqueries columns}, Cmd.none )

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
            ( Loaded <| updateValue id value state, updateCmd endpoints.update encoder id value )

        -- we are beginning an edit
        ( BeginEdit id object, Loaded ({subqueries} as state) ) ->
            ( Loaded {state | modifying = Just <| Editing id <| makeEditInterface subqueries object columns}, Cmd.none )

        -- DELETE

        ( Delete id, Loaded state ) ->
            ( Loaded <| deleteValue id state, deleteCmd endpoints.delete id )

        ( BeginDelete id, Loaded state ) ->
            ( Loaded {state | modifying = Just <| Deleting id}, Cmd.none )

        -- Modal operations

        ( UpdateField key newValue, Loaded state ) ->
            updateField key newValue state

        ( CloseModal, Loaded state ) ->
            ( Loaded {state | modifying = Nothing}, Cmd.none )

        -- Elm moment
        -- I don't specifically have a case for Acked, because I don't think it's very important
        _ ->
            ( model, Cmd.none )


-- as the `delete` request goes through, this function updates the clientside to reflect the change
-- this does mean that, theoretically, "phantom deletes" could happen, where the clientside is
-- updated and the object is disappeared but the object continues to exist in the real database.
-- this is obviously a problem, but i dont really care to do much about it
deleteValue : Int -> State a -> State a
deleteValue id ({data} as state)  =
    { state
    | modifying = Nothing
    , data = Dict.remove id data
    }


-- the command for pinging the delete endpoint
deleteCmd : Endpoint -> Int -> Cmd (Msg a)
deleteCmd endpoint id =
    EvanHttp.delete
        { url = endpoint
        , body = Http.jsonBody <| Enc.int id
        , expect = Http.expectWhatever Acked
        }


-- given a key, input type, and the current state, modifies the user input
-- this function is called basically every time the user enters data into a field
-- in one of the modals
updateField : String -> InputType -> State a -> ( Model a, Cmd (Msg a) )
updateField key newValue ({modifying} as state) =
    ( Loaded
        { state
        | modifying =
            case modifying of
                Just (Editing id interface) ->
                    Just <| Editing id <| Dict.insert key newValue interface

                Just (Creating interface) ->
                    Just <| Creating <| Dict.insert key newValue interface

                _ ->
                    modifying
        }
    , Cmd.none
    )


-- when the `update` PATCH request goes through, this function updates the user interface
-- to display the updated values on the client-side
updateValue : Int -> a -> State a -> State a
updateValue id value ({data} as state) =
    { state
    | modifying = Nothing
    , data = Dict.insert id value data
    }


-- the command that runs whenever the "Submit" button is pressed in the edit modal
updateCmd : Endpoint -> (a -> Value) -> Int -> a -> Cmd (Msg a)
updateCmd endpoint encode id value =
    EvanHttp.patch
        { url = endpoint
        , body = Http.jsonBody <| Enc.object [ ( "id", Enc.int id ), ( "value", encode value ) ]
        , expect = Http.expectWhatever Acked
        }


-- the command that runs whenever the "Submit" button is pressed in the create modal
createCmd : Endpoint -> (a -> Value) -> a -> Cmd (Msg a)
createCmd endpoint encode value = 
    Http.post
        { url = endpoint
        , body = Http.jsonBody <| encode value
        , expect = Http.expectJson (Created value) Dec.int
        }


-- inserts a new value into the table that didn't exist before
-- the new value came from an Http acknowledgement from the server, because we don't know
-- what ID it's supposed to have.
--
-- the server is supposed to provide an ID that is guaranteed to be unused
insertNewValue : a -> State a -> Result Http.Error Int -> ( Model a, Cmd (Msg a) )
insertNewValue value ({data} as state) =
    handleRequest
        <| \ id -> Loaded {state | data = Dict.insert id value data}


-- returns the interface state for the creation interface
makeCreateInterface : Subqueries -> UnzippedColumns a -> Interface
makeCreateInterface =
    toEmptyInput >> makeInterface


-- function that takes an HOF to convert a given writable cellview into an input type,
-- filters all the columns down to only writables, then maps the HOF over the second values,
-- then converts the result into a dict to get the final state for the modal
makeInterface : (WritableCellView a -> InputType) -> UnzippedColumns a -> Interface
makeInterface initialInterface =
    zipColumns
        >> filterMapToWritable
        >> List.map (Tuple.mapSecond initialInterface)
        >> Dict.fromList


-- create an input with empty fields based on the given subqueries and known writable views
toEmptyInput : Subqueries -> WritableCellView a -> InputType
toEmptyInput subqueries w =
    case w of
        WProjection _ tt ->
            Text tt ""

        WSubquery key _ ->
            DropDown (Dict.get key subqueries |> Maybe.withDefault Dict.empty) Nothing


-- returns the interface state for the creation interface
makeEditInterface : Subqueries -> a -> UnzippedColumns a -> Interface
makeEditInterface subqueries object =
    makeInterface <| populateEditInput subqueries object


-- populate a particular input with the data that it should have given the input object
populateEditInput : Subqueries -> a -> WritableCellView a -> InputType
populateEditInput subqueries object w =
    case w of
        WProjection f tt ->
            Text tt <| f object

        WSubquery key f ->
            Just (f object)
                |> DropDown (Dict.get key subqueries |> Maybe.withDefault Dict.empty)


-- filters a List of Columns down to a List of WritableColumns
filterMapToWritable : List (Column a) -> List (WritableColumn a)
filterMapToWritable =
    List.filterMap writableColumn


-- converts a Column to a WritableColumn, returning Nothing in the case it is not writable
writableColumn : Column a -> Maybe (WritableColumn a)
writableColumn column =
    case column.view of
        Projection f (Just ( key, tt )) ->
            Just ( key, WProjection f tt)

        Subquery key f (Just ikey) ->
           Just ( ikey, WSubquery key f )

        _ ->
           Nothing 
            

-- this function runs whenever a request that contains the "topic data" dict comes in while
-- the server is loading
--
-- it essentially finishes loading if the client has received all of the subquery acknowledgements,
-- or fails if an error occurs
loadData : PartialModel a -> Result Http.Error (Dict Int a) -> ( Model a, Cmd (Msg a) )
loadData ({subqueries, subqueriesToFulfill} as partialData) =
    handleRequest
        <| \ xs ->
            case subqueriesToFulfill of
                0 ->
                    Loaded
                        { subqueries = subqueries
                        , data = xs
                        , modifying = Nothing
                        }

                _ ->
                    PartiallyLoaded
                        {partialData | data = Just xs}


-- this function runs whenever a request that contains subquery data comes in while the server is loading
--
-- it finishes loading if the client has received all of the other subquery acknowledgements and the "topic data"
-- is also already loaded
loadSubquery : PartialModel a -> String -> Result Http.Error (Dict Int String) -> ( Model a, Cmd (Msg a) )
loadSubquery ({subqueries, data, subqueriesToFulfill} as partialData) key =
    handleRequest
        <| \ xs ->
            case ( data, subqueriesToFulfill ) of
                ( Just ys, 1 ) ->
                    Loaded
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
-- it takes a higher-order function with which to convert the request result data
-- into a `Model a`, and automatically handles failure
handleRequest : (x -> Model a) -> Result Http.Error x -> ( Model a, Cmd (Msg a) )
handleRequest handler result =
    ( case result of
        Err e ->
            Error <| EvanHttp.httpErrorToString e

        Ok x ->
            handler x

    , Cmd.none
    )


-- VIEW


-- function to determine what we will render
view : Decoder a -> UnzippedColumns a -> Model a -> Html (Msg a)
view decoder {headers, views} model =
    case model of
        PartiallyLoaded _ ->
            Html.p [] [Html.text "PartiallyLoaded"]

        Error msg ->
            Html.p [] [Html.text msg]

        Loaded {subqueries, data, modifying} ->
            Html.div []
                [ creationButton
                , makeTable headers views subqueries data
                , modifying
                    |> Maybe.map (makeModal decoder headers)
                    |> Maybe.withDefault EvanHtml.empty
                ]


-- given the decoder, the list of headers, and the current modification status,
-- determines which modal to display
-- (the possibility of there being no modal is excluded if this function has run)
makeModal : Decoder a -> List String -> Modifying -> Html (Msg a)
makeModal decoder headers m =
    case m of
        Editing id interface ->
            makeEditModal decoder headers id interface
        
        Creating interface ->
            makeCreateModal decoder headers interface

        Deleting id ->
            makeDeleteModal id


-- given the decoder, the list of headers, the id of the editing topic object, and the current
-- state of the modal interface, create the HTML for the edit modal
makeEditModal : Decoder a -> List String -> Int -> Interface -> Html (Msg a)
makeEditModal decoder headers id interface =
    modal
        { header = "Editing"
        , footer = "Submit"
        , onSubmit = nuclearWaste decoder interface <| Update id
        , body = interfaceBody headers interface
        }


-- given the decoder, the list of headers, and the state of the modal interface,
-- create the HTML for the create modal
makeCreateModal : Decoder a -> List String -> Interface -> Html (Msg a)
makeCreateModal decoder headers interface =
    modal
        { header = "Creating"
        , footer = "Submit"
        , onSubmit = nuclearWaste decoder interface Create
        , body = interfaceBody headers interface
        }


-- given the id of the topic object, generates the HTML for the delete modal
makeDeleteModal : Int -> Html (Msg a)
makeDeleteModal id =
    modal
        { header = "Deleting"
        , footer = "Confirm"
        , onSubmit = Just <| Delete id
        , body = [Html.h1 [class "title"] [Html.text "Are you sure?"]]
        }


-- a function for validating the user input that comes from the edit/creation modals
-- returns Nothing if validation fails
nuclearWaste : Decoder a -> Interface -> (a -> Msg a) -> Maybe (Msg a)
nuclearWaste decoder interface msg =
    Enc.dict identity encodeInput interface
        |> Dec.decodeValue decoder
        |> Result.toMaybe
        |> Maybe.map msg


-- encodes an InputType value into a JSON Value
encodeInput : InputType -> Value
encodeInput inputType =
    case inputType of
        Text _ value ->
            Enc.string value

        DropDown _ m ->
            Maybe.map Enc.int m
                |> Maybe.withDefault Enc.null


-- given the list of headers and the state of the modal interface, returns an HTML body to be used
-- in the generic modal function
interfaceBody : List String -> Interface -> List (Html (Msg a))
interfaceBody headers =
    Dict.toList
        >> List.map2 formField headers


-- converts the data regarding a single form field into HTML
formField : String -> ( String, InputType ) -> Html (Msg a)
formField label ( key, contents ) =
    Html.div [class "field"]
        [ Html.label [class "field-label"] [Html.text label]
        , Html.div [class "field-body"]
            [formInput key contents]
        ]


-- converts the data regarding a particular input type into HTML
formInput : String -> InputType -> Html (Msg a)
formInput key contents =
    case contents of
        DropDown domain value ->
            makeDropdown key domain value

        Text tt value ->
            case tt of
                TextArea ->
                    Html.textarea
                        [ class "textarea"
                        , Attrs.type_ "text"
                        , Attrs.value value
                        , Events.onInput <| updateTextInterface key TextArea
                        ]
                        []

                Date ->
                    Html.input
                        [ class "input"
                        , Attrs.type_ "date"
                        , Attrs.value value
                        , Events.onInput <| updateTextInterface key Date
                        ]
                        []

                Input p ->
                    Html.input
                        [ Attrs.type_ "text"
                        , Attrs.value value
                        , Events.onInput <| updateTextInterface key <| Input p
                        , classList
                            [ ( "input", True )
                            , ( "is-danger", not <| p <| value )
                            ]
                        ]
                        []
                        

-- this function serves to update the value of a text input
updateTextInterface : String -> TextType -> String -> Msg a
updateTextInterface key textType =
    Text textType
        >> UpdateField key


-- given the interface key, domain space, and current selected option,
-- renders any dropdown components in a modal
makeDropdown : String -> Dict Int String -> Maybe Int -> Html (Msg a)
makeDropdown key domain value =
    Html.div [class "dropdown"]
        [ Html.div [class "dropdown-trigger"]
            [ Html.button [class "button", Aria.hasPopup, Aria.controls "dropdown-menu"]
                [ Html.span []
                    [ Html.text
                        ( value
                            |> Maybe.andThen ( \ k -> Dict.get k domain )
                            |> Maybe.withDefault "Select..."
                        )
                    ]
                , Html.span [class "icon", class "is-small"]
                    [Html.i [class "fas", class "fa-angle-down", Aria.hidden] []]
                ]
            ]
        , Html.div [class "dropdown-menu", id "dropdown-menu", Aria.role "menu"]
            [ Html.div [class "dropdown-content"]
                <| populateDropdown key domain
            ]
        ]


-- this populates a dropdown's options
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


-- creates the creation button
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
makeTable : List String -> List (CellView a) -> Subqueries -> Dict Int a -> Html (Msg a)
makeTable headers views subqueries data =
    Html.table [class "table"]
        [ Html.thead []
            <| List.map (EvanHtml.wrapElement <| Html.th []) headers
                ++ [Html.th [] [Html.text "Action"]]
        , Html.tbody []
            <| List.map (toRow views subqueries) (Dict.toList data)
        ]


-- converts a particular element into a row in the main data table
toRow : List (CellView a) -> Subqueries -> ( Int, a ) -> Html (Msg a)
toRow views subqueries ( id, object ) =
    Html.tr []
        <| List.map (EvanHtml.wrapElement <| Html.td []) (projectWholeObject subqueries object views)
            ++ [ Html.td []
                    [ Html.a [class "js-modal-trigger", Events.onClick <| BeginEdit id object]
                        [Html.text "Edit"]
                    , Html.br [] []
                    , Html.a [class "has-text-danger", class "js-modal-trigger"]
                        [Html.text "Delete"]
                    ]
                ]


-- converts an object into a row on the table
projectWholeObject : Subqueries -> a -> List (CellView a) -> List String
projectWholeObject subqueries object =
    List.map <| projectObject subqueries object


-- converts an object into a cell on the table
projectObject : Subqueries -> a -> CellView a -> String
projectObject subqueries object cellView =
    case cellView of
        Projection f _ ->
            f object

        Subquery key f _ ->
            Dict.get key subqueries
                |> Maybe.andThen (Dict.get <| f object)
                |> Maybe.withDefault ""
        

type alias ModalOptions a =
    { header : String
    , body : List (Html (Msg a))
    , footer : String
    , onSubmit : Maybe (Msg a)
    }


-- given a set of options, generates the HTML for a modal dialog
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
                , Html.button [class "delete", Aria.label "close", Events.onClick CloseModal] []
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


-- SUBSCRIPTIONS


-- in real life, if i wanted e.g. keyboard input (which in some reality I probably would),
-- i would put those sorts of things here
subscriptions : Model a -> Sub msg
subscriptions =
    always Sub.none


-- main function, and the primary export of this module
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
main {read, create, edit, delete, subqueries, columns, encoder, decoder} =
    let
        unzippedColumns = unzipColumns columns
    in
        Browser.element
            { init = init {read = read, subqueries = subqueries, decoder = decoder}
            , view = view decoder unzippedColumns
            , update = update {create = create, update = edit, delete = delete} encoder unzippedColumns
            , subscriptions = subscriptions
            }
