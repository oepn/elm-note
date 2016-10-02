module Main exposing (..)

import Debug
import Dict exposing (Dict)
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Markdown


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { notes : Notes
    , activeNote : NoteId
    , isEditing : Bool
    }


type alias Notes =
    Dict NoteId Note


type alias NoteId =
    Int


type alias Note =
    { title : String
    , body : Markdown
    }


type alias Markdown =
    String


init : ( Model, Cmd Msg )
init =
    ( Model initialPosts (firstKey initialPosts ? 1) False
    , Cmd.none
    )


initialPosts : Notes
initialPosts =
    Dict.fromList <|
        [ ( 2, Note "Lorem Ipsum" """# Test
Maecenas *feugiat* at elit sed gravida.
""" )
        , ( 3, Note "Dolor Set" """Phasellus tincidunt fringilla rutrum.""" )
        ]


emptyNote : Note
emptyNote =
    Note "" ""



-- UPDATE


type Msg
    = SetActive NoteId
    | ToggleEditing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetActive id ->
            ( { model | activeNote = id }, Cmd.none )

        ToggleEditing ->
            ( { model | isEditing = not model.isEditing }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "section" ]
        [ css "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.2.1/css/bulma.min.css"
        , div [ class "container" ]
            [ div [ class "columns" ]
                [ div [ class "column is-one-third" ] [ noteList model.notes ]
                , div [ class "column is-two-thirds" ] [ activeNote model ]
                ]
            ]
        ]


css : String -> Html msg
css path =
    node "link" [ rel "stylesheet", href path ] []


noteList : Notes -> Html Msg
noteList notes =
    nav [ class "panel" ]
        [ h1 [ class "panel-heading" ] [ text "Notes" ]
        , div [] (notes |> Dict.toList |> List.map noteListEntry)
        ]


noteListEntry : ( NoteId, Note ) -> Html Msg
noteListEntry ( id, { title, body } ) =
    a [ class "panel-block", onClick (SetActive id) ]
        [ text title
        ]


activeNote : Model -> Html Msg
activeNote { notes, activeNote, isEditing } =
    let
        { title, body } =
            Dict.get activeNote notes ? emptyNote

        noteContent : String -> Html msg
        noteContent =
            case isEditing of
                True ->
                    \str -> p [ class "control" ] [ textarea [ class "textarea" ] [ text str ] ]

                False ->
                    Markdown.toHtml [ class "content" ]

        buttonTitle : String
        buttonTitle =
            if isEditing then
                "Save"
            else
                "Edit"
    in
        div [ class "card is-fullwidth" ]
            [ header [ class "card-header" ]
                [ h2 [ class "card-header-title" ] [ text title ]
                ]
            , div [ class "card-content" ]
                [ noteContent body
                , p [ class "control" ]
                    [ button [ class "button", onClick ToggleEditing ]
                        [ text buttonTitle ]
                    ]
                ]
            ]



-- HELPERS


(?) : Maybe a -> a -> a
(?) maybe default =
    Maybe.withDefault default maybe


firstKey : Dict comparable v -> Maybe comparable
firstKey =
    Dict.toList >> List.map fst >> List.head
