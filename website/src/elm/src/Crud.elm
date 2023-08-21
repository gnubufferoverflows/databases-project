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


module Crud exposing
    ( Flags
    , Model
    , Msg
    , app
    , Column
    , Endpoint
    , CellView(..)
    , TextType(..)
    , AppType
    )


import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attrs exposing (class, id, classList)
import Html.Events as Events
import Http
import Json.Decode as Dec exposing (Decoder)
import Json.Encode as Enc exposing (Value)


import EESE
import EESE.Html as EvanHtml
import EESE.Html.Aria as Aria
import EESE.Http as EvanHttp
import EESE.Json.Decode as EvanJsonDecoders


type alias Flags =
    ()


-- type to represent the state of the crud app
-- Crud is either loading, in error state, or running
type Model comparable a
    = PartiallyLoaded (PartialModel comparable a)
    | Error String
    | Loaded (State comparable a)


-- represents the partially loaded model while data is still coming in
type alias PartialModel comparable a =
    { subqueries : Subqueries
    , data : Maybe (Dict comparable a)
    , subqueriesToFulfill : Int
    }


-- this type comes up a lot whenever subqueries are involved, so I aliased it
type alias Subqueries =
    Dict String (Dict Int String)


-- the state of the webpage, once everything is up and running
type alias State comparable a =
    { subqueries : Subqueries -- a list of string-keyed subquery results for enumerated table cells
    , data : Dict comparable a -- the collection of entities operated upon
    , modifying : Maybe (Modifying comparable) -- the state of editing the entities
    }


-- type to represent the state of modifying the page
-- either we are not modifying anything, we are editing a particular entity,
-- or we are creating a new entity
type Modifying comparable
    = Editing comparable Interface
    | Creating Interface
    | Deleting comparable


-- internal storage for user interface for creating and editing entities
-- the first value of the value tuple is the header (title), the second is the stateful data
type alias Interface =
    Dict String ( String, InputType )


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
type Msg comparable a
    -- Create
    = CreateWithoutId a
    | CreateWithId comparable a
    | BeginCreate
    | Created a (Result Http.Error comparable)
    -- Read
    | GotData (Result Http.Error (Dict comparable a))
    | GotSubquery String (Result Http.Error (Dict Int String))
    -- Update
    | Update comparable a
    | BeginEdit comparable a
    -- Delete
    | Delete comparable
    | BeginDelete comparable
    -- Modal operations
    | UpdateField String InputType
    | CloseModal
    -- Server communication
    | Acked (Result Http.Error ())
    

-- endpoint type alias
type alias Endpoint =
    String


-- the column type. represents a column in the topic table
type alias Column comparable a =
    { header : String -- the name of the column
    , view : CellView comparable a -- the kind of view of the object that the column will provide
    }


-- cellview represents how to display the data associated with a particular column
type CellView comparable a
    = Projection (a -> String) String TextType -- a function from the object to a String representation of some aspect of it
    | Subquery String (a -> Int) String -- a subquery key that will be looked up in the subquery dictionary
    | Id (comparable -> String) -- the id of the value, with a function to stringify it
    | Preset String -- a preset string


-- a type for storing the writable subset of cellview in a more convenient manner
type WritableCellView a
    = WProjection (a -> String) TextType
    | WSubquery String (a -> Int)


type alias WritableColumn a =
    ( String, WritableCellView a )


-- unzipped dataview, for efficient access of necessary subcomponents
-- this ensures that every list in the dataview is the same length,
-- impossible to enforce otherwise
type alias UnzippedColumns comparable a =
    { headers : List String
    , views : List (CellView comparable a)
    }


-- unzips a list of columns into a record with multiple lists for easier processing. the inverse of zipColumns
unzipColumns : List (Column comparable a) -> UnzippedColumns comparable a
unzipColumns =
    List.foldr columnCons emptyUnzippedColumns


-- the inverse of unzipColumns
zipColumns : UnzippedColumns comparable a -> List (Column comparable a)
zipColumns {headers, views} =
    List.map2 Column headers views


-- given a column and an UnzippedColumns object, append the elements of that column to each of the UnzippedColumns object's element lists
columnCons : Column comparable a -> UnzippedColumns comparable a -> UnzippedColumns comparable a
columnCons column {headers, views} =
    {headers = column.header :: headers, views = column.view :: views}


-- represents the empty UnzippedColumns object
emptyUnzippedColumns : UnzippedColumns comparable a
emptyUnzippedColumns =
    {headers = [], views = []}


-- INIT


-- a type alias for the list of configuration data passed to init, as this type of list pops up more than once
type alias InitStuff comparable a =
    { read : Endpoint
    , subqueries : Dict String Endpoint
    , keyDecoder : Decoder comparable
    , valueDecoder : Decoder a
    }


-- initial state and loading side-effect
init : InitStuff comparable a -> Flags -> ( Model comparable a, Cmd (Msg comparable a) )
init initstuff =
    always
        ( Dict.size initstuff.subqueries |> unloaded |> PartiallyLoaded
        , fetchAllData initstuff
        )


-- the unloaded state
unloaded : Int -> PartialModel comparable a
unloaded size =
    { subqueries = Dict.empty
    , data = Nothing
    , subqueriesToFulfill = size
    }


-- fetch the main data and all subqueries
-- performs this task concurrently
fetchAllData : InitStuff comparable a -> Cmd (Msg comparable a)
fetchAllData {read, subqueries, keyDecoder, valueDecoder} =
    Cmd.batch
        <| fetchMainData read keyDecoder valueDecoder :: fetchSubqueries subqueries


-- gets the main data, the stuff that will populate the main table
fetchMainData : Endpoint -> Decoder comparable -> Decoder a -> Cmd (Msg comparable a)
fetchMainData endpoint keyDecoder valueDecoder =
    Http.get
        { url = endpoint
        , expect = Http.expectJson GotData <| EvanJsonDecoders.arbitraryDict keyDecoder valueDecoder
        }


-- spins up all of the subqueries into a list of commands to be batched
fetchSubqueries : Dict String Endpoint -> List (Cmd (Msg comparable a))
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


type alias EncoderPair comparable a =
    { key : comparable -> Value
    , value : a -> Value
    }


type alias CustomizableIdValue =
    { label : String
    , subquery : String
    , interfaceKey : String
    }


type alias UpdateStuff comparable a =
    { endpoints : CrudEndpoints
    , columns : UnzippedColumns comparable a
    , encoders : EncoderPair comparable a
    , keyDecoder : Decoder comparable
    , customizableIdValues : List CustomizableIdValue
    }


-- main update loop
-- what will we do when we get a new message?
-- this function determines the result of that,
-- and manages all side effects
update :
   UpdateStuff comparable a
   -> Msg comparable a
   -> Model comparable a
   -> ( Model comparable a, Cmd (Msg comparable a) )
update config msg model =
    case ( msg, model ) of
        -- CREATE

        ( CreateWithoutId value, Loaded state ) ->
            ( Loaded {state | modifying = Nothing}, createCmd config.endpoints.create config.encoders.value config.keyDecoder value )

        ( CreateWithId id value, Loaded state ) ->
            ( Loaded <| insertNewValueWithKnownId id value state, createWithId config.endpoints.create config.encoders id value )

        ( BeginCreate, Loaded ({subqueries} as state) ) ->
            ( Loaded {state | modifying = Just <| Creating <| makeCreateInterface subqueries config.customizableIdValues config.columns}, Cmd.none )

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
            ( Loaded <| updateValue id value state, updateCmd config.endpoints.update config.encoders id value )

        -- we are beginning an edit
        ( BeginEdit id object, Loaded ({subqueries} as state) ) ->
            ( Loaded {state | modifying = Just <| Editing id <| makeEditInterface subqueries object config.columns}, Cmd.none )

        -- DELETE

        ( Delete id, Loaded state ) ->
            ( Loaded <| deleteValue id state, deleteCmd config.endpoints.delete config.encoders.key id )

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


createWithId : Endpoint -> EncoderPair comparable a -> comparable -> a -> Cmd (Msg comparable a)
createWithId endpoint encoders id value =
    Http.post
        { url = endpoint
        , body = Http.jsonBody <| Enc.object [ ( "id", encoders.key id ), ( "value", encoders.value value ) ]
        , expect = Http.expectWhatever Acked
        }


-- this function works like insertNewValue, but it assumes that we already know the ID we want to insert
insertNewValueWithKnownId : comparable -> a -> State comparable a -> State comparable a
insertNewValueWithKnownId id value ({data} as state) =
    { state
    | modifying = Nothing
    , data = Dict.insert id value data
    }


-- as the `delete` request goes through, this function updates the clientside to reflect the change
-- this does mean that, theoretically, "phantom deletes" could happen, where the clientside is
-- updated and the object is disappeared but the object continues to exist in the real database.
-- this is obviously a problem, but i dont really care to do much about it
deleteValue : comparable -> State comparable a -> State comparable a
deleteValue id ({data} as state)  =
    { state
    | modifying = Nothing
    , data = Dict.remove id data
    }


-- the command for pinging the delete endpoint
deleteCmd : Endpoint -> (comparable -> Value) -> comparable -> Cmd (Msg comparable a)
deleteCmd endpoint encoder id =
    EvanHttp.delete
        { url = endpoint
        , body = Http.jsonBody <| encoder id
        , expect = Http.expectWhatever Acked
        }


-- given a key, input type, and the current state, modifies the user input
-- this function is called basically every time the user enters data into a field
-- in one of the modals
updateField : String -> InputType -> State comparable a -> ( Model comparable a, Cmd (Msg comparable a) )
updateField key newValue ({modifying} as state) =
    ( Loaded
        { state
        | modifying =
            case modifying of
                Just (Editing id interface) ->
                    Just <| Editing id <| updateFieldValue key newValue interface

                Just (Creating interface) ->
                    Just <| Creating <| updateFieldValue key newValue interface

                _ ->
                    modifying
        }
    , Cmd.none
    )


updateFieldValue : String -> InputType -> Interface -> Interface
updateFieldValue key newValue =
    Dict.update key
        <| Maybe.map (Tuple.mapSecond <| always newValue) >> Maybe.withDefault ( key, newValue ) >> Just


-- when the `update` PATCH request goes through, this function updates the user interface
-- to display the updated values on the client-side
updateValue : comparable -> a -> State comparable a -> State comparable a
updateValue id value ({data} as state) =
    { state
    | modifying = Nothing
    , data = Dict.insert id value data
    }


-- the command that runs whenever the "Submit" button is pressed in the edit modal
updateCmd : Endpoint -> EncoderPair comparable a -> comparable -> a -> Cmd (Msg comparable a)
updateCmd endpoint encoders id value =
    EvanHttp.patch
        { url = endpoint
        , body = Http.jsonBody <| Enc.object [ ( "id", encoders.key id ), ( "value", encoders.value value ) ]
        , expect = Http.expectWhatever Acked
        }


-- the command that runs whenever the "Submit" button is pressed in the create modal
createCmd : Endpoint -> (a -> Value) -> Decoder comparable -> a -> Cmd (Msg comparable a)
createCmd endpoint encode decoder value = 
    Http.post
        { url = endpoint
        , body = Http.jsonBody <| Enc.object [ ( "value", encode value ) ]
        , expect = Http.expectJson (Created value) decoder
        }


-- inserts a new value into the table that didn't exist before
-- the new value came from an Http acknowledgement from the server, because we don't know
-- what ID it's supposed to have.
--
-- the server is supposed to provide an ID that is guaranteed to be unused
insertNewValue : a -> State comparable a -> Result Http.Error comparable -> ( Model comparable a, Cmd (Msg comparable a) )
insertNewValue value ({data} as state) =
    handleRequest
        <| \ id -> Loaded {state | data = Dict.insert id value data}


-- returns the interface state for the creation interface
makeCreateInterface : Subqueries -> List CustomizableIdValue -> UnzippedColumns comparable a -> Interface
makeCreateInterface subqueries customizableIdValues unzippedColumns =
    Dict.union (idInterface subqueries customizableIdValues) (unzippedColumns |> makeInterface (toEmptyInput subqueries))


idInterface : Subqueries -> List CustomizableIdValue -> Interface
idInterface subqueries =
    Dict.empty
        |> List.foldl
            ( \ {label, subquery, interfaceKey} ->
                Dict.insert interfaceKey
                    ( label, DropDown (Dict.get subquery subqueries |> Maybe.withDefault Dict.empty) Nothing )
            )
    

-- create an input with empty fields based on the given subqueries and known writable views
toEmptyInput : Subqueries -> WritableCellView a -> InputType
toEmptyInput subqueries w =
    case w of
        WProjection _ tt ->
            Text tt ""

        WSubquery key _ ->
            DropDown (Dict.get key subqueries |> Maybe.withDefault Dict.empty) Nothing


-- returns the interface state for the creation interface
makeEditInterface : Subqueries -> a -> UnzippedColumns comparable a -> Interface
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


-- function that takes an HOF to convert a given writable cellview into an input type,
-- filters all the columns down to only writables, then maps the HOF over the second values,
-- then converts the result into a dict to get the final state for the modal
makeInterface : (WritableCellView a -> InputType) -> UnzippedColumns comparable a -> Interface
makeInterface initialInterface =
    zipColumns
        >> filterMapToWritable
        >> List.map (Tuple.mapSecond <| Tuple.mapSecond initialInterface)
        >> Dict.fromList


-- filters a List of Columns down to a List of WritableColumns
filterMapToWritable : List (Column comparable a) -> List ( String, WritableColumn a )
filterMapToWritable =
    List.filterMap writableColumn


-- converts a Column to a WritableColumn, returning Nothing in the case it is not writable
writableColumn : Column comparable a -> Maybe ( String, WritableColumn a )
writableColumn column =
    case column.view of
        Projection f key tt ->
            Just ( key, ( column.header, WProjection f tt ) )

        Subquery key f ikey ->
            Just ( ikey, ( column.header, WSubquery key f ) )

        _ ->
           Nothing 
            

-- this function runs whenever a request that contains the "topic data" dict comes in while
-- the server is loading
--
-- it essentially finishes loading if the client has received all of the subquery acknowledgements,
-- or fails if an error occurs
loadData : PartialModel comparable a -> Result Http.Error (Dict comparable a) -> ( Model comparable a, Cmd (Msg comparable a) )
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
loadSubquery : PartialModel comparable a -> String -> Result Http.Error (Dict Int String) -> ( Model comparable a, Cmd (Msg comparable a) )
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
-- into a `Model comparable a`, and automatically handles failure
handleRequest : (x -> Model comparable a) -> Result Http.Error x -> ( Model comparable a, Cmd (Msg comparable a) )
handleRequest handler result =
    ( case result of
        Err e ->
            Error <| EvanHttp.httpErrorToString e

        Ok x ->
            handler x

    , Cmd.none
    )


-- VIEW


type alias ViewStuff comparable a =
    { keyDecoder : Maybe (Decoder comparable)
    , valueDecoder : Decoder a
    , columns : UnzippedColumns comparable a
    , title : String
    }


-- function to determine what we will render
view : ViewStuff comparable a -> Model comparable a -> Browser.Document (Msg comparable a)
view config model =
    { title = config.title
    , body = viewBody config model
    }


viewBody : ViewStuff comparable a -> Model comparable a -> List (Html (Msg comparable a))
    case model of
        PartiallyLoaded _ ->
            [EvanHtml.empty]

        Error msg ->
            [Html.p [] [Html.text msg]]

        Loaded {subqueries, data, modifying} ->
            [ creationButton
            , makeTable config.columns.headers config.columns.views subqueries data
            , modifying
                |> Maybe.map (makeModal config.keyDecoder config.valueDecoder)
                |> Maybe.withDefault EvanHtml.empty
            ]


-- given the decoder, the list of headers, and the current modification status,
-- determines which modal to display
-- (the possibility of there being no modal is excluded if this function has run)
makeModal : Maybe (Decoder comparable) -> Decoder a -> Modifying comparable -> Html (Msg comparable a)
makeModal keyDecoder valueDecoder m =
    case m of
        Editing id interface ->
            makeEditModal valueDecoder id interface
        
        Creating interface ->
            makeCreateModal keyDecoder valueDecoder interface

        Deleting id ->
            makeDeleteModal id


-- given the decoder, the list of headers, the id of the editing topic object, and the current
-- state of the modal interface, create the HTML for the edit modal
makeEditModal : Decoder a -> comparable -> Interface -> Html (Msg comparable a)
makeEditModal decoder id interface =
    modal
        { header = "Editing"
        , footer = "Submit"
        , onSubmit = nuclearWaste decoder interface <| Update id
        , body = interfaceBody interface
        }


-- given the decoder, the list of headers, and the state of the modal interface,
-- create the HTML for the create modal
makeCreateModal : Maybe (Decoder comparable) -> Decoder a -> Interface -> Html (Msg comparable a)
makeCreateModal mKeyDecoder valueDecoder interface =
    modal
        { header = "Creating"
        , footer = "Submit"
        , onSubmit =
            case mKeyDecoder of
                Nothing ->
                    nuclearWaste valueDecoder interface CreateWithoutId

                Just keyDecoder ->
                    nuclearWasteWithId keyDecoder valueDecoder interface CreateWithId

        , body = interfaceBody interface
        }


nuclearWasteWithId : Decoder comparable -> Decoder a -> Interface -> (comparable -> a -> Msg comparable a) -> Maybe (Msg comparable a)
nuclearWasteWithId keyDecoder valueDecoder interface msg =
    let
        encodedInterface = Enc.dict identity (Tuple.second >> encodeInput) interface
    in
        EvanJsonDecoders.decodeValueMaybe keyDecoder encodedInterface
            |> Maybe.andThen -- this is one of Elm's cruftiest shortcomings, no "do" syntax
                ( \ key ->
                    EvanJsonDecoders.decodeValueMaybe valueDecoder encodedInterface
                        |> Maybe.map (msg key)
                )


-- a function for validating the user input that comes from the edit/creation modals
-- returns Nothing if validation fails
nuclearWaste : Decoder a -> Interface -> (a -> Msg comparable a) -> Maybe (Msg comparable a)
nuclearWaste decoder interface msg =
    Enc.dict identity (Tuple.second >> encodeInput) interface
        |> EvanJsonDecoders.decodeValueMaybe decoder
        |> Maybe.map msg


-- given the id of the topic object, generates the HTML for the delete modal
makeDeleteModal : comparable -> Html (Msg comparable a)
makeDeleteModal id =
    modal
        { header = "Deleting"
        , footer = "Confirm"
        , onSubmit = Just <| Delete id
        , body = [Html.h1 [class "title"] [Html.text "Are you sure?"]]
        }


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
interfaceBody :  Interface -> List (Html (Msg comparable a))
interfaceBody =
    Dict.toList
        >> List.map formField


-- converts the data regarding a single form field into HTML
formField : ( String, ( String, InputType ) ) -> Html (Msg comparable a)
formField ( key, ( label, contents ) ) =
    Html.div [class "field"]
        [ Html.label [class "field-label"] [Html.text label]
        , Html.div [class "field-body"]
            [formInput key contents]
        ]


-- converts the data regarding a particular input type into HTML
formInput : String -> InputType -> Html (Msg comparable a)
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
updateTextInterface : String -> TextType -> String -> Msg comparable a
updateTextInterface key textType =
    Text textType
        >> UpdateField key


-- given the interface key, domain space, and current selected option,
-- renders any dropdown components in a modal
makeDropdown : String -> Dict Int String -> Maybe Int -> Html (Msg comparable a)
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
populateDropdown : String -> Dict Int String -> List (Html (Msg comparable a))
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
creationButton : Html (Msg comparable a)
creationButton =
    Html.button
        [ class "button"
        , class "is-primary"
        , class "js-modal-trigger"
        , Events.onClick BeginCreate
        ]
        []


-- creates the main data table
makeTable : List String -> List (CellView comparable a) -> Subqueries -> Dict comparable a -> Html (Msg comparable a)
makeTable headers views subqueries data =
    Html.table [class "table"]
        [ Html.thead []
            <| List.map (EvanHtml.wrapElement <| Html.th []) headers
                ++ [Html.th [] [Html.text "Action"]]
        , Html.tbody []
            <| List.map (toRow views subqueries) (Dict.toList data)
        ]


-- converts a particular element into a row in the main data table
toRow : List (CellView comparable a) -> Subqueries -> ( comparable, a ) -> Html (Msg comparable a)
toRow views subqueries ( id, object ) =
    Html.tr []
        <| List.map (EvanHtml.wrapElement <| Html.td []) (projectWholeObject subqueries id object views)
            ++ [ Html.td []
                    [ if List.all isIneditable views then
                        EvanHtml.empty
                      else
                        Html.a [class "js-modal-trigger", Events.onClick <| BeginEdit id object]
                            [Html.text "Edit"]
                    , Html.br [] []
                    , Html.a [class "has-text-danger", class "js-modal-trigger"]
                        [Html.text "Delete"]
                    ]
                ]


isIneditable : CellView comparable a -> Bool
isIneditable cellView =
    case cellView of
        Id _ ->
            True

        Preset _ ->
            True

        _ ->
            False


-- converts an object into a row on the table
projectWholeObject : Subqueries -> comparable -> a -> List (CellView comparable a) -> List String
projectWholeObject subqueries id object =
    List.map <| projectObject subqueries id object


-- converts an object into a cell on the table
projectObject : Subqueries -> comparable -> a -> CellView comparable a -> String
projectObject subqueries id object cellView =
    case cellView of
        Projection f _ _ ->
            f object

        Subquery key f _ ->
            Dict.get key subqueries
                |> Maybe.andThen (Dict.get <| f object)
                |> Maybe.withDefault ""

        Id f ->
            f id

        Preset x ->
            x
        

type alias ModalOptions comparable a =
    { header : String
    , body : List (Html (Msg comparable a))
    , footer : String
    , onSubmit : Maybe (Msg comparable a)
    }


-- given a set of options, generates the HTML for a modal dialog
modal : ModalOptions comparable a -> Html (Msg comparable a)
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
subscriptions : Model comparable a -> Sub msg
subscriptions =
    always Sub.none


type alias AppType flags model msg =
    { init : flags -> ( model, Cmd msg )
    , view : model -> Browser.Document msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    }


-- main function, and the primary export of this module
app :
    { read : Endpoint
    , create : Endpoint
    , edit : Endpoint
    , delete : Endpoint
    , subqueries : Dict String Endpoint
    , columns : List (Column comparable a)
    , keyDecoder : Decoder comparable
    , valueDecoder : Decoder a
    , keyEncoder : comparable -> Value
    , valueEncoder : a -> Value
    , customizableIdValues : List CustomizableIdValue
    , title : String
    }
    -> AppType Flags (Model comparable a) (Msg comparable a)
app config =
    let
        unzippedColumns = unzipColumns config.columns
    in
        { init =
            init
                { read = config.read
                , subqueries = config.subqueries
                , keyDecoder = config.keyDecoder
                , valueDecoder = config.valueDecoder
                }
        , view =
            view
                { keyDecoder =
                    EESE.guardMaybe (not <| List.isEmpty config.customizableIdValues) <| Just config.keyDecoder
                , valueDecoder = config.valueDecoder
                , columns = unzippedColumns
                , title = config.title
                }
        , update =
            update
                { endpoints =
                    { create = config.create
                    , update = config.edit
                    , delete = config.delete
                    }
                , columns = unzippedColumns
                , encoders = EncoderPair config.keyEncoder config.valueEncoder
                , keyDecoder = config.keyDecoder
                , customizableIdValues = config.customizableIdValues
                }
        , subscriptions = subscriptions
        }
