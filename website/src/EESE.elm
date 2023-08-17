-- Evan's Elm Stdlib Extension


module EESE exposing (filterMapKeys)


import Dict exposing (Dict)


-- like List.filterMap, but over the keys of a dictionary
filterMapKeys : (k -> Maybe comparable) -> Dict k v -> Dict comparable v
filterMapKeys f =
    Dict.empty
        |> Dict.foldl
            ( \ k v ->
                case f k of
                    Just x ->
                        Dict.insert x v

                    Nothing ->
                        identity
            )
