module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Element exposing (centerX, centerY, layout, padding, text)
import Html exposing (Html)
import Http
import Json.Decode exposing (list)
import Loaded
import User exposing (User, userDecoder)



-- MODEL


type Failure
    = HttpError Http.Error
    | ImpossibleStateTransition { model : Model, msg : Msg }


type Model
    = Loading
    | Failed Failure
    | Loaded Loaded.Model


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Http.get { url = "https://jsonplaceholder.typicode.com/users?_limit=3", expect = Http.expectJson GotUsers (list userDecoder) }
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- UPDATE


type Msg
    = GotUsers (Result Http.Error (List User))
    | LoadedMsg Loaded.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GotUsers (Ok users), Loading ) ->
            ( Loaded <| Loaded.init users, Cmd.none )

        ( GotUsers (Err error), Loading ) ->
            ( Failed (HttpError error), Cmd.none )

        ( LoadedMsg message, Loaded model_ ) ->
            Loaded.update message model_
                |> Tuple.mapBoth Loaded
                    (Cmd.map LoadedMsg)

        ( msg_, model_ ) ->
            ( Failed (ImpossibleStateTransition { msg = msg_, model = model_ }), Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            layout [ centerX, centerY, padding 16 ] (Element.text "Loading Users ...")

        Failed _ ->
            layout [ padding 16 ] (Element.text "Error happened")

        Loaded data ->
            Html.map LoadedMsg (Loaded.view data)



-- MAIN


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }
