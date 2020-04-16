module Main exposing (main)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Navbar as Navbar
import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Time
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((</>), Parser, s, top)


type alias Flags =
    {}


type alias Todo =
    { name : String
    , date : Time.Posix
    , isDone : Bool
    , id : Int
    }


type alias Model =
    { navKey : Navigation.Key
    , page : Page
    , navState : Navbar.State
    , todos : List Todo
    , todoText : String
    }


type Page
    = Home
    | NotFound


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = UrlChange
        }


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( navState, navCmd ) =
            Navbar.initialState NavMsg

        ( model, urlCmd ) =
            urlUpdate url
                { navKey = key
                , navState = navState
                , page = Home
                , todos = []
                , todoText = ""
                }
    in
    ( model, Cmd.batch [ urlCmd, navCmd ] )


type Msg
    = UrlChange Url
    | ClickedLink UrlRequest
    | NavMsg Navbar.State
    | AddTodo
    | UpdateText String
    | ToggleTodoDone Int
    | NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
    Navbar.subscriptions model.navState NavMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink req ->
            case req of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl model.navKey <| Url.toString url )

                Browser.External href ->
                    ( model, Navigation.load href )

        UrlChange url ->
            urlUpdate url model

        NavMsg state ->
            ( { model | navState = state }
            , Cmd.none
            )

        AddTodo ->
            ( { model | todos = List.append model.todos [ { name = model.todoText, date = Time.millisToPosix 0, isDone = False, id = List.length model.todos } ] }, Cmd.none )

        UpdateText text ->
            ( { model | todoText = text }, Cmd.none )

        ToggleTodoDone todoID ->
            ( { model | todos = List.map (toggle todoID) model.todos }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


toggle : Int -> Todo -> Todo
toggle id todo =
    if todo.id == id then
        { todo | isDone = not todo.isDone }

    else
        todo


urlUpdate : Url -> Model -> ( Model, Cmd Msg )
urlUpdate url model =
    case decode url of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just route ->
            ( { model | page = route }, Cmd.none )


decode : Url -> Maybe Page
decode url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> UrlParser.parse routeParser


routeParser : Parser (Page -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map Home top
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Elm Bootstrap"
    , body =
        [ div []
            [ menu model
            , mainContent model
            ]
        ]
    }


menu : Model -> Html Msg
menu model =
    Navbar.config NavMsg
        |> Navbar.withAnimation
        |> Navbar.container
        |> Navbar.brand [ href "#" ] [ text "Jake's To-Do list" ]
        |> Navbar.view model.navState


mainContent : Model -> Html Msg
mainContent model =
    Grid.container [] <|
        case model.page of
            Home ->
                pageHome model

            NotFound ->
                pageNotFound


pageHome : Model -> List (Html Msg)
pageHome model =
    [ ListGroup.ul
        (List.map
            (\todo ->
                ListGroup.li []
                    [ Checkbox.checkbox [ Checkbox.attrs [ title todo.name ] ] todo.name
                    ]
            )
            model.todos
        )
    , Form.formInline [ onSubmit NoOp ]
        [ Input.text [ Input.attrs [ onInput UpdateText, placeholder "New To-do...", value model.todoText ] ]
        , Button.button [ Button.primary, Button.attrs [ onClick AddTodo ] ] [ text "Add To-do" ]
        ]
    ]


pageNotFound : List (Html Msg)
pageNotFound =
    [ h1 [] [ text "Not found" ]
    , text "Sorry couldn't find that page"
    ]
