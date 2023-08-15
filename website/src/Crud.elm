module Crud exposing (Model(..), Msg(..), main, Column, Endpoint, CellView(..))


import Browser
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
    { subqueries : Dict String (List String)
    , data : Maybe (Dict Int a)
    , subqueriesToFulfill : Int
    }


type alias State a =
    { subqueries : Dict String (List String) -- a list of string-keyed subquery results for enumerated table cells
    , data : Dict Int a -- the collection of entities operated upon
    , modifying : Modifying a -- the state of editing the entities
    }


-- type to represent the state of modifying the page
-- either we are not modifying anything, we are editing a particular entity,
-- or we are creating a new entity
type Modifying a
    = NotModifying
    | Editing Int Interface
    | Creating Interface


-- internal storage for user interface for creating and editing entities
type alias Interface =
    Dict String String


type Msg a
    -- Create
    = Create Value
    | BeginCreate
    | EndCreate
    -- Read
    | GotData (Result Http.Error (Dict Int a))
    | GotSubquery String (Result Http.Error (List String))
    -- Update
    | Update Int Value
    | BeginEdit a
    | EndEdit
    -- Delete
    | Delete Int
    -- User input
    | UpdateForm String String
    -- Server communication
    | Acked (Result Http.Error ())
    

type alias Endpoint =
    String


type alias Column a =
    { header : String -- the name of the column
    , view : CellView a -- the kind of view of the object that the column will provide
    , parser : Maybe (String -> Maybe String) -- the parser, for editing values. if there is no parser (its value is Nothing), the value is not editable
    }


type CellView a
    = Projection (a -> String) -- a function from the object to a String representation of some aspect of it
    | Subquery String -- a subquery key that will be looked up in the subquery dictionary


-- unzipped dataview, for efficient access of necessary subcomponents
-- this ensures that every list in the dataview is the same length,
-- impossible to enforce otherwise
type alias UnzippedColumns a
    { headers : List String
    , views : List (CellView a)
    , parsers : List (Maybe (String -> Maybe String))
    }


unzipColumns : List (Column a) -> UnzippedColumns a
unzipColumns =
        List.foldr dataViewCons emptyDataView


columnCons : Column a -> UnzippedColumns a -> UnzippedColumns a
columnCons {header, view, parser} {headers, views, parsers} =
    {headers = header :: headers, views = view :: views, parsers = parser :: parsers}


emptyUnzippedColumns : UnzippedColumns a
emptyUnzippedColumns =
    {headers = [], views = [], parsers = []}


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


-- augments a decoder to be over the values of a dict from int to a
decodeIntDict : Decoder a -> Decoder (Dict Int a)
decodeIntDict decoder =
    Dec.map (mapFilterKeys String.toInt)
        <| Dec.dict decoder


-- maps over the keys of a dictionary, filtering out keys that do not parse
mapFilterKeys : (k -> Maybe n) -> Dict k v -> Dict n v
mapFilterKeys f =
    Dict.empty
        |> Dict.foldl (
            \ k v ->
                case f k of
                    Just n ->
                        Dict.insert n v

                    _ ->
                        identity
        )


-- spins up all of the subqueries into a list of commands to be batched
fetchSubqueries : Dict String Endpoint -> List (Cmd (Msg a))
fetchSubqueries =
    Dict.toList
        >> List.map (
            \ ( key, endpoint ) ->
                Html.get
                    { url = endpoint
                    , expect = Http.expectJson (GotSubquery key) <| Dec.list Dec.string
                    }
        )


type alias CrudEndpoints =
    { create : Endpoint
    , edit : Endpoint
    , update : Endpoint
    , delete : Endpoint
    }


update :
    { endpoints : CrudEndpoints }
    -> Msg a
    -> Model a
    -> ( Model a, Cmd (Msg a) )
update {endpoints} =
    case ( msg, model ) of
        ( GotData result, PartiallyLoaded ({subqueries, subqueriesToFulfill} as partialData) ) ->
            result
                |> handleGetRequest (
                    \ xs ->
                        case subqueriesToFulfill of
                            0 ->
                                Running
                                    { subqueries = subqueries
                                    , data = xs
                                    , modifying = NotModifying
                                    }

                            _ ->
                                PartiallyLoaded
                                    {partialData | data = Just xs}
                )

        ( GotSubquery key result, PartiallyLoaded ({subqueries, data, subqueriesToFulfill} as partialData) ) ->
            result
                |> handleGetRequest (
                    \ xs ->
                        case ( data, subqueriesToFulfill ) of
                            ( Just ys, 1 ) ->
                                Running
                                    { subqueries = Dict.insert key xs subqueries
                                    , data = ys
                                    , modifying = NotModifying
                                    }

                            _ ->
                                PartiallyLoaded
                                    { partialData
                                    | subqueries = Dict.insert key xs subqueries
                                    , subqueriesToFulfill = subqueriesToFulfill - 1
                                    }
                )


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
    , pk : a -> Int
    }
    -> Program () (Model a) (Msg a)
main {read, create, edit, delete, columns, decoder, pk} =
