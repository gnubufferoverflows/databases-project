-- Evan's Elm Stdlib Extension: Browser


module EESE.Browser exposing
    ( mapDocument
    )


import Browser
import Html


mapDocument : (a -> msg) -> Browser.Document a -> Browser.Document msg
mapDocument f document =
    { title = document.title
    , body =
        document.body
            |> List.map (Html.map f)
    }
