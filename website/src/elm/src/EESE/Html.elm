-- Evan's Elm Stdlib Extension: Html

module EESE.Html exposing
    ( empty
    , wrapElement
    )

import Html exposing (Html)


-- empty html
empty : Html msg
empty =
    Html.text ""


-- wraps a String as the only text child of an Html element
wrapElement : (List (Html msg) -> Html msg) -> String -> Html msg
wrapElement element =
    Html.text >> List.singleton >> element
