-- Routes the pages
-- Written by Evan


module Route exposing
    ( Route(..)
    , fromUrl
    )


import Url exposing (Url)
import Url.Parser exposing (Parser, (</>), s)


import EESE


type Route
    = Home
    | Cats
    | Diagnoses
    -- | Veterinarians
    -- | Diseases
    -- | Medications
    -- | Prescriptions
    -- | Cat Int
    -- | Adopt Int
    -- | AdoptAll


parser : Parser (Route -> a) a
parser =
    Url.Parser.oneOf
        <| List.map (EESE.uncurry Url.Parser.map)
            [ ( Home, Url.Parser.top )
            , ( Cats, s "cats" )
            , ( Diagnoses, s "diagnoses" )
            -- , ( Veterinarians, s "veterinarians" )
            -- , ( Diseases, s "diseases" )
            -- , ( Medications, s "medications" )
            -- , ( Prescriptions, s "prescriptions" )
            -- , ( Cat, s "cat" </> Url.Parser.int )
            -- , ( Adopt, s "adopt" </> Url.Parser.int )
            -- , ( AdoptAll, s "adopt" </> s "all" )
            ]


fromUrl : Url -> Maybe Route
fromUrl =
    Url.Parser.parse parser
