module User exposing (User, userDecoder)

import Json.Decode as Decode exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (hardcoded, required)
import RemoteData exposing (WebData)
import Todo exposing (Todo)


type alias User =
    { id : Int
    , name : String
    , todos : WebData (List Todo)
    }


userDecoder : Decoder User
userDecoder =
    Decode.succeed
        User
        |> required "id" int
        |> required "username" string
        |> hardcoded RemoteData.NotAsked
