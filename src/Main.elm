module Main exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Markdown
import Navigation
import String


main : Program Never
main =
    Navigation.program urlParser
        { init = init
        , view = view
        , update = update
        , urlUpdate = urlUpdate
        , subscriptions = subscriptions
        }



-- URL PARSERS


toUrl : NoteId -> String
toUrl id =
    "#/" ++ toString id


fromUrl : String -> Result String NoteId
fromUrl url =
    String.toInt (String.dropLeft 2 url)


urlParser : Navigation.Parser (Result String NoteId)
urlParser =
    Navigation.makeParser (fromUrl << .hash)



-- MODEL


type alias Model =
    { notes : Notes
    , activeId : NoteId
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


init : Result String NoteId -> ( Model, Cmd Msg )
init result =
    let
        activeId =
            case result of
                Ok id ->
                    id

                Err _ ->
                    firstKey initialNotes ? 1
    in
        ( Model initialNotes activeId False, Cmd.none )


initialNotes : Notes
initialNotes =
    Dict.fromList <|
        [ ( 2, Note "Lorem Ipsum" """# Test
Maecenas *feugiat* at elit sed gravida.""" )
        , ( 3, Note "Dolor Set" """Phasellus tincidunt fringilla rutrum.""" )
        ]


emptyNote : Note
emptyNote =
    Note "" ""



-- UPDATE


type Msg
    = SetActive NoteId
    | ToggleEditing
    | UpdateTitle NoteId String
    | UpdateBody NoteId Markdown


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetActive id ->
            ( { model | activeId = id }, Navigation.newUrl (toUrl id) )

        ToggleEditing ->
            ( { model | isEditing = not model.isEditing }, Cmd.none )

        UpdateTitle id title ->
            ( { model | notes = updateNote id (updateTitle title) model.notes }
            , Cmd.none
            )

        UpdateBody id body ->
            ( { model | notes = updateNote id (updateBody body) model.notes }
            , Cmd.none
            )


updateNote : NoteId -> (Maybe Note -> Maybe Note) -> Notes -> Notes
updateNote id alter notes =
    Dict.update id alter notes


updateTitle : String -> Maybe Note -> Maybe Note
updateTitle newTitle note =
    Maybe.map (\note -> { note | title = newTitle }) note


updateBody : Markdown -> Maybe Note -> Maybe Note
updateBody newBody note =
    Maybe.map (\note -> { note | body = newBody }) note


urlUpdate : Result String NoteId -> Model -> ( Model, Cmd Msg )
urlUpdate result model =
    case result of
        Ok newId ->
            ( { model | activeId = newId }, Cmd.none )

        Err _ ->
            ( model, Navigation.modifyUrl (toUrl model.activeId) )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "section" ]
        [ css "./bulma.css"
        , div [ class "container" ]
            [ div [ class "columns" ]
                [ div
                    [ visibleWhen (not model.isEditing) "column is-one-third" ]
                    [ noteList model.notes ]
                , div [ class "column" ]
                    [ note model ]
                ]
            ]
        ]


noteList : Notes -> Html Msg
noteList notes =
    nav [ class "panel" ]
        [ h1 [ class "panel-heading" ] [ text "Notes" ]
        , div [] (notes |> Dict.toList |> List.map noteListEntry)
        ]


noteListEntry : ( NoteId, Note ) -> Html Msg
noteListEntry ( id, { title, body } ) =
    a [ class "panel-block", onClick (SetActive id) ] [ text title ]


note : Model -> Html Msg
note { notes, activeId, isEditing } =
    let
        note : Note
        note =
            Dict.get activeId notes ? emptyNote
    in
        div [ class "columns" ]
            [ div [ class "column" ]
                [ div [ class "card is-fullwidth" ]
                    [ noteViewer note
                    , footer [ visibleWhen (not isEditing) "card-footer" ]
                        [ noteButton isEditing ]
                    ]
                ]
            , div [ visibleWhen isEditing "column is-half" ]
                [ div [ class "card is-fullwidth" ]
                    [ noteEditor activeId note
                    , footer [ class "card-footer" ]
                        [ noteButton isEditing ]
                    ]
                ]
            ]


noteButton : Bool -> Html Msg
noteButton isEditing =
    let
        buttonTitle : String
        buttonTitle =
            if isEditing then
                "Save"
            else
                "Edit"
    in
        a [ class "card-footer-item", onClick ToggleEditing ]
            [ text buttonTitle ]


noteViewer : Note -> Html Msg
noteViewer { title, body } =
    div []
        [ header [ class "card-header" ]
            [ h2 [ class "card-header-title" ] [ text title ] ]
        , div [ class "card-content" ]
            [ Markdown.toHtmlWith markdownOptions [ class "content" ] body ]
        ]


markdownOptions : Markdown.Options
markdownOptions =
    let
        options =
            Markdown.defaultOptions
    in
        { options | sanitize = True }


noteEditor : NoteId -> Note -> Html Msg
noteEditor noteId { title, body } =
    div [ class "card-content" ]
        [ label [ class "label" ] [ text "Title" ]
        , p [ class "control" ]
            [ input
                [ type' "text"
                , class "input"
                , value title
                , onInput (UpdateTitle noteId)
                ]
                []
            ]
        , label [ class "label" ] [ text "Body" ]
        , p [ class "control" ]
            [ textarea
                [ class "textarea"
                , onInput (UpdateBody noteId)
                ]
                [ text body ]
            ]
        ]



-- VIEW HELPERS


css : String -> Html msg
css path =
    node "link" [ rel "stylesheet", href path ] []


visibleWhen : Bool -> String -> Attribute msg
visibleWhen show classes =
    let
        newClasses =
            if show then
                classes
            else
                classes ++ " is-hidden"
    in
        class newClasses



-- HELPERS


(?) : Maybe a -> a -> a
(?) maybe default =
    Maybe.withDefault default maybe


firstKey : Dict comparable v -> Maybe comparable
firstKey =
    Dict.toList >> List.map fst >> List.head
