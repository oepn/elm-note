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
    , activeNote : Note
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
        ( { notes = initialNotes
          , activeId = activeId
          , activeNote = getNote activeId initialNotes
          , isEditing = False
          }
        , Cmd.none
        )


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
    | UpdateTitle String
    | UpdateBody Markdown
    | SaveActiveNote


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetActive id ->
            ( { model
                | activeId = id
                , activeNote = getNote id model.notes
              }
            , Navigation.newUrl (toUrl id)
            )

        ToggleEditing ->
            ( { model | isEditing = not model.isEditing }
            , Cmd.none
            )

        UpdateTitle title ->
            ( { model | activeNote = updateTitle title model.activeNote }
            , Cmd.none
            )

        UpdateBody body ->
            ( { model | activeNote = updateBody body model.activeNote }
            , Cmd.none
            )

        SaveActiveNote ->
            ( { model
                | notes = updateNote model.activeId model.activeNote model.notes
                , isEditing = False
              }
            , Cmd.none
            )


getNote : NoteId -> Notes -> Note
getNote id notes =
    Dict.get id notes ? emptyNote


updateNote : NoteId -> Note -> Notes -> Notes
updateNote id newNote notes =
    Dict.update id (\_ -> Just newNote) notes


updateTitle : String -> Note -> Note
updateTitle newTitle note =
    { note | title = newTitle }


updateBody : Markdown -> Note -> Note
updateBody newBody note =
    { note | body = newBody }


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
note { notes, activeId, activeNote, isEditing } =
    let
        previewNote : Note
        previewNote =
            if isEditing then
                activeNote
            else
                Dict.get activeId notes ? emptyNote
    in
        div [ class "columns" ]
            [ div [ class "column" ]
                [ div [ class "card is-fullwidth" ]
                    [ noteViewer previewNote
                    , footer [ visibleWhen (not isEditing) "card-footer" ]
                        [ noteButton "Edit" ToggleEditing ]
                    ]
                ]
            , div [ visibleWhen isEditing "column is-half" ]
                [ div [ class "card is-fullwidth" ]
                    [ noteEditor activeNote
                    , footer [ class "card-footer" ]
                        [ noteButton "Cancel" ToggleEditing
                        , noteButton "Save" SaveActiveNote
                        ]
                    ]
                ]
            ]


noteButton : String -> Msg -> Html Msg
noteButton title msg =
    a [ class "card-footer-item", onClick msg ]
        [ text title ]


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


noteEditor : Note -> Html Msg
noteEditor { title, body } =
    div [ class "card-content" ]
        [ label [ class "label" ] [ text "Title" ]
        , p [ class "control" ]
            [ input
                [ type' "text"
                , class "input"
                , value title
                , onInput UpdateTitle
                ]
                []
            ]
        , label [ class "label" ] [ text "Body" ]
        , p [ class "control" ]
            [ textarea
                [ class "textarea"
                , onInput UpdateBody
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
