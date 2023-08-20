-- Evan's Elm Stdlib Extension -- Http Edition


module EESE.Http exposing
    ( patch
    , delete
    , httpErrorToString
    )


import Http


type alias PostyParameters msg =
    { url : String
    , body : Http.Body
    , expect : Http.Expect msg
    }


-- implements a simple PATCH request
-- in the form of an object { url : String, body : Http.Body, expect : Http.Expect }
patch : PostyParameters msg -> Cmd msg
patch r =
    Http.request
        { method = "PATCH"
        , headers = []
        , url = r.url
        , body = r.body
        , expect = r.expect
        , timeout = Nothing
        , tracker = Nothing
        }


-- implements a simple DELETE request, using the parameters
-- in the form of an object { url : String, body : Http.Body, expect : Http.Expect }
delete : PostyParameters msg -> Cmd msg
delete r =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = r.url
        , body = r.body
        , expect = r.expect
        , timeout = Nothing
        , tracker = Nothing
        }


-- converts an Http.Error to String
httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl x ->
            "The url at " ++ x ++ " could not be located."

        Http.Timeout ->
            "Timed out."

        Http.NetworkError ->
            "A network error occurred with your connection."

        Http.BadStatus x ->
            "Bad status: " ++ String.fromInt x ++ "."

        Http.BadBody x ->
            "Bad body: " ++ x ++ "."
