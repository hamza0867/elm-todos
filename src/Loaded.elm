module Loaded exposing (Model, Msg(..), init, update, view)

import Element exposing (column, el, html, layout, padding, paddingEach, paddingXY, spacing, text)
import Element.Input as Input exposing (labelAbove, option, text)
import Html exposing (Html)
import Http
import Json.Decode exposing (list)
import RemoteData exposing (WebData)
import Todo exposing (Todo, todoDecoder)
import User exposing (User)



-- MODEL


type alias Model =
    { users : List User
    , selectedUser : Maybe User
    }


init : List User -> Model
init users =
    { users = users
    , selectedUser = Nothing
    }



-- UPDATE


type Msg
    = UpdateSelectedUser (Maybe User)
    | GotTodos User (WebData (List Todo))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateSelectedUser selectedUser ->
            case selectedUser of
                Just user ->
                    case user.todos of
                        RemoteData.Success _ ->
                            ( { model
                                | selectedUser = selectedUser
                              }
                            , Cmd.none
                            )

                        _ ->
                            let
                                newUser =
                                    { user | todos = RemoteData.Loading }
                            in
                            ( { model
                                | selectedUser = Just newUser
                                , users =
                                    model.users
                                        |> List.map
                                            (\user_ ->
                                                if user_ == user then
                                                    newUser

                                                else
                                                    user_
                                            )
                              }
                            , Http.get { url = "https://jsonplaceholder.typicode.com/todos?_limit=3&userId=" ++ (user.id |> String.fromInt), expect = Http.expectJson (RemoteData.fromResult >> GotTodos newUser) (list todoDecoder) }
                            )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        GotTodos user webTodos ->
            ( let
                newUser =
                    { user | todos = webTodos }
              in
              { model
                | users =
                    model.users
                        |> List.map
                            (\user_ ->
                                if user_ == user then
                                    newUser

                                else
                                    user_
                            )
                , selectedUser = Just newUser
              }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    layout [ padding 16 ] <|
        column [ spacing 16 ]
            [ Input.radio [ spacing 8, paddingXY 16 0 ]
                { onChange = \val -> UpdateSelectedUser (Just val)
                , selected = model.selectedUser
                , label =
                    labelAbove
                        [ paddingEach
                            { top = 0
                            , bottom = 8
                            , right = 0
                            , left = 0
                            }
                        ]
                        (Element.text "Select a user :")
                , options =
                    model.users
                        |> List.map
                            (\user ->
                                option user <| Element.text user.name
                            )
                }
            , el [] <| Element.text "Todos"
            , column [ paddingXY 16 0 ]
                [ html <|
                    (model.selectedUser
                        |> Maybe.map
                            (\user ->
                                case user.todos of
                                    RemoteData.Success todos ->
                                        Html.ul []
                                            (todos
                                                |> List.map
                                                    (\todo ->
                                                        Html.li []
                                                            [ Html.text todo.title
                                                            ]
                                                    )
                                            )

                                    RemoteData.Failure _ ->
                                        Html.text "There was some error"

                                    RemoteData.Loading ->
                                        Html.text "Loading todos ..."

                                    RemoteData.NotAsked ->
                                        Html.text "This should not happen"
                            )
                        |> Maybe.withDefault (Html.text "Please select a user to show todos")
                    )
                ]
            ]
