module Main (..) where

import Effects exposing (Never)
import Html exposing (..)
import Html.Attributes exposing (..)
import Autocomplete exposing (initWithClasses, initItem, update, view)
import StartApp
import Task exposing (Task)
import Http
import String


fetchMoreItems : String -> Task Effects.Never (List Autocomplete.Item)
fetchMoreItems url =
  Http.url url []
    |> Http.getString
    |> Task.toMaybe
    |> Task.map responseToItems


responseToItems : Maybe String -> List Autocomplete.Item
responseToItems maybeString =
  case maybeString of
    Just string ->
      String.lines string
        |> List.indexedMap (\i itemText -> initItem (toString i) itemText)

    Nothing ->
      []


testData : List Autocomplete.Item
testData =
  []


initExampleClassListConfig : Autocomplete.ClassListConfig
initExampleClassListConfig =
  { menu = [ ( "autocomplete-menu-default", True ) ]
  , item = [ ( "autocomplete-item-default", True ) ]
  , selectedItem = [ ( "autocomplete-selected-item-default", True ) ]
  , list = [ ( "autocomplete-list-default", True ) ]
  , input = [ ( "autocomplete-input-default", True ) ]
  }


getItemsTask : String -> Int -> Task Effects.Never (List Autocomplete.Item)
getItemsTask value index =
  fetchMoreItems "https://raw.githubusercontent.com/first20hours/google-10000-english/master/20k.txt"


initAutocomplete : ( Autocomplete.Autocomplete, Effects.Effects Autocomplete.Action )
initAutocomplete =
  initWithClasses testData 5 getItemsTask initExampleClassListConfig
    |> Autocomplete.customizeLoading (img [ src "assets/loading.svg" ] [])


app : StartApp.App Autocomplete.Autocomplete
app =
  StartApp.start
    { init = initAutocomplete
    , update = update
    , view = view
    , inputs = []
    }


main : Signal Html.Html
main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
