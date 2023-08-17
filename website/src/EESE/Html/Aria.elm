-- Evan's Elm Stdlib Extension: Html Aria properites
--
-- This module exposes definitions for a variety of aria properties,
-- as such things may be useful

module EESE.Html.Aria exposing
    ( role
    , label
    , hidden
    , hasPopup
    , controls
    )


import Html exposing (Attribute)
import Html.Attributes


role : String -> Attribute msg
role =
    Html.Attributes.attribute "role"


label : String -> Attribute msg
label =
    Html.Attributes.attribute "aria-label"


hidden : Attribute msg
hidden =
    Html.Attributes.attribute "aria-hidden" "true"


hasPopup : Attribute msg
hasPopup =
    Html.Attributes.attribute "aria-haspopup" "true"


controls : String -> Attribute msg
controls =
    Html.Attributes.attribute "aria-controls"
