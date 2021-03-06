module Main exposing (..)

import Autocomplete.Config
import Autocomplete exposing (Autocomplete)
import Autocomplete.Styling as Styling
import Html exposing (..)
import Html.Attributes exposing (style, class)
import String
import Html.App as Html exposing (map)

getClasses : Styling.View -> Styling.Classes
getClasses view =
  case view of
    Styling.Menu ->
      [ ( "autocomplete-menu", True ) ]

    Styling.List ->
      [ ( "autocomplete-list", True ) ]

    Styling.Item ->
      [ ( "autocomplete-item", True ) ]

    Styling.SelectedItem ->
      [ ( "autocomplete-selected-item", True ) ]

    Styling.Input ->
      [ ( "autocomplete-input", True ) ]


type alias Model =
  { autocompleteRemaining : String
  , autocomplete : Autocomplete
  , value : String
  , showMenu : Bool
  }


init : Model
init =
  let
    config =
      Autocomplete.Config.defaultConfig
        |> Autocomplete.Config.setClassesFn getClasses
        |> Autocomplete.Config.setItemHtml getItemHtml
  in
    { autocompleteRemaining = ""
    , autocomplete = Autocomplete.initWithConfig [ "elm", "makes", "coding", "life", "easy" ] config
    , value = ""
    , showMenu = False
    }


type Msg
  = Autocomplete Autocomplete.Msg
  | SetValue String
  | ShowMenu Bool


update : Msg -> Model -> Model
update action model =
  case action of
    Autocomplete act ->
      let
        ( updatedAutocomplete, completed ) =
          Autocomplete.update act model.autocomplete

        preview =
          Autocomplete.getSelectedItem updatedAutocomplete
      in
        { model
          | autocompleteRemaining =
              preview
                |> String.slice (String.length (Autocomplete.getCurrentValue updatedAutocomplete)) (String.length preview)
          , autocomplete = updatedAutocomplete
        }

    SetValue value ->
      { model | value = value }

    ShowMenu bool ->
      { model | showMenu = bool, autocomplete = Autocomplete.showMenu bool model.autocomplete }


view : Model -> Html Msg
view model =
  div
    [ style [ ( "position", "relative" ), ( "font-family", "monospace" ), ( "font-size", "12px" ) ] ]
    [ span
        [ style [ ( "position", "absolute" ), ( "left", "3px" ), ( "top", "3px" ) ] ]
        [ span [ style [ ( "visibility", "none" ) ] ] [ text (Autocomplete.getCurrentValue model.autocomplete) ]
        , span [ style [ ( "color", "gray" ) ] ] [ text model.autocompleteRemaining ]
        ]
    , map Autocomplete (Autocomplete.view model.autocomplete)
    ]


getItemHtml : String -> Html a
getItemHtml text' =
  div
    [ style [ ( "display", "flex" ), ( "justify-content", "space-between" ) ] ]
    [ span [] [ text text' ]
    , span [] [ text "😍" ]
    ]


main : Program Never
main =
  Html.beginnerProgram
    { model = init
    , update = update
    , view = view
    }
