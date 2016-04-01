module Main (..) where

import Autocomplete.Simple as Autocomplete
import Autocomplete.Simple exposing (Item, ClassListConfig, Action, initWithClasses, initItem, getSelectedItemText)
import StartApp.Simple
import Html exposing (..)
import Html.Attributes exposing (style)
import String


testData : List Item
testData =
  [ initItem "0" "elm"
  , initItem "1" "makes"
  , initItem "2" "coding"
  , initItem "3" "life"
  , initItem "4" "easy"
  ]


initExampleClassListConfig : ClassListConfig
initExampleClassListConfig =
  { menu = [ ( "autocomplete-menu-default", True ) ]
  , item = [ ( "autocomplete-item-default", True ) ]
  , selectedItem = [ ( "autocomplete-selected-item-default", True ) ]
  , list = [ ( "autocomplete-list-default", True ) ]
  , input = [ ( "autocomplete-input-default", True ) ]
  }


type alias Model =
  { autocompleteRemaining : String
  , autocomplete : Autocomplete.Autocomplete
  }


init : Model
init =
  { autocompleteRemaining = ""
  , autocomplete = initWithClasses testData 5 initExampleClassListConfig
  }


type Action
  = Autocomplete Autocomplete.Simple.Action


update : Action -> Model -> Model
update action model =
  case action of
    Autocomplete act ->
      let
        updatedAutocomplete =
          Autocomplete.update act model.autocomplete

        preview =
          getSelectedItemText updatedAutocomplete
      in
        { model
          | autocompleteRemaining =
              preview
                |> String.slice (String.length updatedAutocomplete.value) (String.length preview)
          , autocomplete = updatedAutocomplete
        }


view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ style [ ( "position", "relative" ), ( "font-family", "monospace" ), ( "font-size", "12px" ) ] ]
    [ span
        [ style [ ( "position", "absolute" ), ( "left", "3px" ), ( "top", "3px" ) ] ]
        [ span [ style [ ( "visibility", "none" ) ] ] [ text model.autocomplete.value ]
        , span [ style [ ( "color", "gray" ) ] ] [ text model.autocompleteRemaining ]
        ]
    , Autocomplete.view (Signal.forwardTo address Autocomplete) model.autocomplete
    ]


main : Signal Html.Html
main =
  StartApp.Simple.start
    { model = init
    , update = update
    , view = view
    }
