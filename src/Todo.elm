module Todo exposing (Todo, todoDecoder)

import Json.Decode as Decode exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (required)


type alias Todo =
    { id : Int
    , title : String
    }


todoDecoder : Decoder Todo
todoDecoder =
    Decode.succeed
        Todo
        |> required "id" int
        |> required "title" string
