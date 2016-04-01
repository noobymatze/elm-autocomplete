module Autocomplete.Simple (Autocomplete, Item, ClassListConfig, ClassList, init, initWithClasses, initItem, Action, update, view, getSelectedItemText) where

{-| A customizable autocomplete component.

The autocomplete consists of a menu, a list, list items, and an input.
All of the aforementioned are styleable via css classes.

The currently selected item is preserved.

Selection is modified by keyboard input, mouse clicks,
and is also styled via css classes.

# Definition
@docs Autocomplete, Item, ClassListConfig, ClassList

# Creating an Autocomplete
@docs init, initWithClasses, initItem, customizeNoMatches

# Update
@docs Action, update

# Views
@docs view

# Helpers
@docs getSelectedItemText

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (..)
import String exposing (..)
import Styling exposing (getStyling, ClassConfig, Classes)


{-| The Autocomplete model.
    It assumes filtering is based upon strings.
-}
type alias Autocomplete =
  { value : String
  , items : List Item
  , maxListSize : Int
  , filteredItems : List Item
  , filterFn : Item -> String -> Bool
  , compareFn : Item -> Item -> Order
  , selectedItemIndex : Index
  , classes : Maybe ClassListConfig
  , noMatchesDisplay : Html
  }


type alias Index =
  Int


{-| A collection of class names attributed to each piece of the component.
-}
type alias ClassListConfig =
  ClassConfig


{-| Alias for the argument to an elm-html classList
-}
type alias ClassList =
  Classes


{-| A possible selection in the autocomplete.
-}
type alias Item =
  { key : ID
  , text : Text
  }


type alias ID =
  String


type alias Text =
  String


{-| Creates an Autocomplete from a list of items with a default `String.startsWith` filter
-}
init : List Item -> Int -> Autocomplete
init items maxSize =
  { value = ""
  , items = items
  , maxListSize = maxSize
  , filteredItems = items
  , filterFn = (\item value -> String.startsWith value item.text)
  , compareFn = normalComparison
  , selectedItemIndex = 0
  , classes = Nothing
  , noMatchesDisplay = p [] [ text "No Matches" ]
  }


{-| Creates an Autocomplete with custom class names
-}
initWithClasses : List Item -> Int -> ClassListConfig -> Autocomplete
initWithClasses items maxSize classListConfig =
  let
    model =
      init items maxSize
  in
    { model | classes = Just classListConfig }


{-| Add some custom HTML to display when there are no matches
-}
customizeNoMatches : Html -> Autocomplete -> Autocomplete
customizeNoMatches noMatchesHtml model =
  { model | noMatchesDisplay = noMatchesHtml }


{-| Creates an Autocomplete Item
-}
initItem : ID -> Text -> Item
initItem id text =
  { key = id
  , text = text
  }


normalComparison : Item -> Item -> Order
normalComparison item1 item2 =
  case compare item1.text item2.text of
    LT ->
      LT

    EQ ->
      EQ

    GT ->
      GT


type Action
  = NoOp
  | SetValue String
  | Complete
  | ChangeSelection Int


{-| The quintessential Elm Architecture reducer.
-}
update : Action -> Autocomplete -> Autocomplete
update action model =
  case action of
    NoOp ->
      model

    SetValue value ->
      if value == "" then
        { model
          | value = value
          , filteredItems =
              model.items
                |> List.sortWith model.compareFn
        }
      else
        { model
          | value = value
          , filteredItems =
              List.filter (\item -> model.filterFn item value) model.items
                |> List.sortWith model.compareFn
        }

    Complete ->
      let
        selectedItem =
          List.drop model.selectedItemIndex model.filteredItems
            |> List.head
      in
        case selectedItem of
          Just item ->
            { model | value = item.text }

          Nothing ->
            model

    ChangeSelection newIndex ->
      let
        boundedNewIndex =
          Basics.max newIndex 0
            |> Basics.min ((List.length model.filteredItems) - 1)
            |> Basics.min (model.maxListSize - 1)
      in
        { model | selectedItemIndex = boundedNewIndex }


{-| The full Autocomplete view, with menu and input.
    Needs a Signal.Address and Autocomplete (typical of the Elm Architecture).
-}
view : Address Action -> Autocomplete -> Html
view address model =
  div
    []
    [ viewInput address model
    , if List.isEmpty model.filteredItems then
        model.noMatchesDisplay
      else
        viewMenu address model
    ]


viewInput : Address Action -> Autocomplete -> Html
viewInput address model =
  let
    handleKeyDown code =
      case code of
        38 ->
          ChangeSelection (model.selectedItemIndex - 1)

        40 ->
          ChangeSelection (model.selectedItemIndex + 1)

        9 ->
          Complete

        _ ->
          NoOp
  in
    input
      [ type' "text"
      , on "input" targetValue (Signal.message address << SetValue)
      , on "keydown" keyCode (\code -> Signal.message address (handleKeyDown code))
      , value model.value
      , classList (getStyling model.classes Styling.Input).classes'
      , (getStyling model.classes Styling.Input).inlineStyle
      ]
      []


viewItem : Signal.Address Action -> Autocomplete -> Item -> Index -> Html
viewItem address model item index =
  li
    [ id item.key
    , classList (getStyling model.classes Styling.Item).classes'
    , (getStyling model.classes Styling.Item).inlineStyle
    , onMouseEnter address (ChangeSelection index)
    ]
    [ text item.text ]


viewSelectedItem : Autocomplete -> Item -> Html
viewSelectedItem model item =
  li
    [ classList (getStyling model.classes Styling.SelectedItem).classes'
    , (getStyling model.classes Styling.SelectedItem).inlineStyle
    ]
    [ text item.text ]


viewMenu : Signal.Address Action -> Autocomplete -> Html
viewMenu address model =
  div
    [ classList (getStyling model.classes Styling.Menu).classes'
    , (getStyling model.classes Styling.Menu).inlineStyle
    ]
    [ viewList address model ]


viewList : Signal.Address Action -> Autocomplete -> Html
viewList address model =
  let
    getItemView index item =
      if index == model.selectedItemIndex then
        viewSelectedItem model item
      else
        viewItem address model item index
  in
    ul
      [ classList (getStyling model.classes Styling.List).classes'
      , (getStyling model.classes Styling.List).inlineStyle
      ]
      (List.indexedMap getItemView model.filteredItems)



-- Helpers


getSelectedItem : Autocomplete -> Maybe Item
getSelectedItem model =
  List.drop model.selectedItemIndex model.filteredItems
    |> List.head


{-| Get the text of the currently selected item
-}
getSelectedItemText : Autocomplete -> Text
getSelectedItemText model =
  case (getSelectedItem model) of
    Just item ->
      item.text

    Nothing ->
      model.value
