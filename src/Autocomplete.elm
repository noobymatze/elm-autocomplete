module Autocomplete (Autocomplete, Item, ClassListConfig, ClassList, init, initWithClasses, initItem, initItemCustomHtml, customizeNoMatches, customizeLoading, Action, update, view) where

{-| A customizable autocomplete component.

The autocomplete consists of a menu, a list, list items, and an input.
All of the aforementioned are styleable via css classes.

The currently selected item is preserved.

Selection is modified by keyboard input, mouse clicks,
and is also styled via css classes.

# Definition
@docs Autocomplete, Item, ClassListConfig, ClassList

# Creating an Autocomplete
@docs init, initWithClasses, initItem, initItemCustomHtml, customizeNoMatches, customizeLoading

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
import Effects exposing (Effects)
import Signal
import String exposing (..)
import Task exposing (Task)
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
  , getItemsTask : GetItemsTask
  , selectedItemIndex : Index
  , classes : Maybe ClassListConfig
  , noMatchesDisplay : Html
  , loadingDisplay : Html
  , showLoading : Bool
  , showMenu : Bool
  }


type alias GetItemsTask =
  String -> Index -> Task Effects.Never (List Item)


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
  , html : Html
  }


type alias ID =
  String


type alias Text =
  String


{-| Creates an Autocomplete from a list of items with a default `String.startsWith` filter
-}
init : List Item -> Int -> GetItemsTask -> ( Autocomplete, Effects Action )
init items maxListSize getItemsTask =
  ( { value = ""
    , items = items
    , maxListSize = maxListSize
    , filteredItems = items
    , filterFn = (\item value -> String.startsWith value item.text)
    , compareFn = normalComparison
    , getItemsTask = getItemsTask
    , selectedItemIndex = 0
    , classes = Nothing
    , noMatchesDisplay = p [] [ text "No Matches" ]
    , loadingDisplay = p [] [ text "..." ]
    , showLoading = False
    , showMenu = False
    }
  , Effects.none
  )


{-| Creates an Autocomplete with custom class names
-}
initWithClasses : List Item -> Int -> GetItemsTask -> ClassListConfig -> ( Autocomplete, Effects Action )
initWithClasses items maxListSize getItemsTask classListConfig =
  ( { value = ""
    , items = items
    , filteredItems = items
    , maxListSize = maxListSize
    , filterFn = (\item value -> String.startsWith value item.text)
    , compareFn = normalComparison
    , getItemsTask = getItemsTask
    , selectedItemIndex = 0
    , classes = Just classListConfig
    , noMatchesDisplay = p [] [ text "No Matches" ]
    , loadingDisplay = p [] [ text "..." ]
    , showLoading = False
    , showMenu = False
    }
  , Effects.none
  )


{-| Add some custom HTML to display when there are no matches
-}
customizeNoMatches : Html -> ( Autocomplete, Effects Action ) -> ( Autocomplete, Effects Action )
customizeNoMatches noMatchesHtml tup =
  let
    model =
      fst tup
  in
    ( { model | noMatchesDisplay = noMatchesHtml }, snd tup )


{-| Add some custom HTML to display when on the initial load
-}
customizeLoading : Html -> ( Autocomplete, Effects Action ) -> ( Autocomplete, Effects Action )
customizeLoading loadingHtml tup =
  let
    model =
      fst tup
  in
    ( { model | loadingDisplay = loadingHtml }, snd tup )


{-| Creates an Autocomplete Item
-}
initItem : ID -> Text -> Item
initItem id text' =
  { key = id
  , text = text'
  , html = text text'
  }


{-| Creates an Autocomplete Item with custom read-only Html
-}
initItemCustomHtml : ID -> Text -> Html -> Item
initItemCustomHtml id text' html =
  { key = id
  , text = text'
  , html = html
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
  | UpdateItems (List Item)
  | Complete
  | ChangeSelection Int
  | ShowMenu Bool


{-| The quintessential Elm Architecture reducer.
-}
update : Action -> Autocomplete -> ( Autocomplete, Effects Action )
update action model =
  case action of
    NoOp ->
      ( model, Effects.none )

    SetValue value ->
      updateInputValue value model

    UpdateItems items ->
      ( { model
          | items = items
          , filteredItems =
              List.filter (\item -> model.filterFn item model.value) model.items
                |> List.sortWith model.compareFn
          , showLoading = False
        }
      , Effects.none
      )

    Complete ->
      case (getSelectedItem model) of
        Just item ->
          ( { model | value = item.text, showMenu = False }, Effects.none )

        Nothing ->
          ( model, Effects.none )

    ChangeSelection newIndex ->
      let
        boundedNewIndex =
          Basics.max newIndex 0
            |> Basics.min ((List.length model.filteredItems) - 1)
            |> Basics.min (model.maxListSize - 1)
      in
        ( { model | selectedItemIndex = boundedNewIndex }, Effects.none )

    ShowMenu bool ->
      ( { model | showMenu = bool }, Effects.none )


{-| The full Autocomplete view, with menu and input.
    Needs a Signal.Address and Autocomplete (typical of the Elm Architecture).
-}
view : Signal.Address Action -> Autocomplete -> Html
view address model =
  div
    [ onBlur address (ShowMenu False) ]
    [ viewInput address model
    , if not model.showMenu then
        div [] []
      else if model.showLoading then
        model.loadingDisplay
      else if List.isEmpty model.filteredItems then
        model.noMatchesDisplay
      else
        viewMenu address model
    ]


viewInput : Signal.Address Action -> Autocomplete -> Html
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
      , onFocus address (ShowMenu True)
      , value model.value
      , classList (getStyling model.classes Styling.Input).classes'
      , (getStyling model.classes Styling.Input).inlineStyle
      , autocomplete True
      ]
      []


viewItem : Signal.Address Action -> Autocomplete -> Item -> Index -> Html
viewItem address model item index =
  li
    [ classList (getStyling model.classes Styling.Item).classes'
    , (getStyling model.classes Styling.Item).inlineStyle
    , onMouseEnter address (ChangeSelection index)
    ]
    [ item.html ]


viewSelectedItem : Signal.Address Action -> Autocomplete -> Item -> Html
viewSelectedItem address model item =
  li
    [ classList (getStyling model.classes Styling.SelectedItem).classes'
    , (getStyling model.classes Styling.SelectedItem).inlineStyle
    , onClick address Complete
    ]
    [ item.html ]


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
        viewSelectedItem address model item
      else
        viewItem address model item index
  in
    ul
      [ classList (getStyling model.classes Styling.List).classes'
      , (getStyling model.classes Styling.List).inlineStyle
      ]
      (List.indexedMap getItemView model.filteredItems
        |> List.take model.maxListSize
      )



-- Effects


getMoreItems : String -> Autocomplete -> Effects Action
getMoreItems value model =
  model.getItemsTask value model.selectedItemIndex
    |> Task.map UpdateItems
    |> Effects.task



-- Helpers


updateInputValue : Text -> Autocomplete -> ( Autocomplete, Effects Action )
updateInputValue text model =
  if text == "" then
    ( { model
        | value = text
        , filteredItems =
            model.items
              |> List.sortWith model.compareFn
        , selectedItemIndex = 0
      }
    , Effects.none
    )
  else
    let
      filteredItems =
        List.filter (\item -> model.filterFn item text) model.items
          |> List.sortWith model.compareFn

      showLoading =
        if List.isEmpty filteredItems then
          True
        else
          False
    in
      ( { model
          | value = text
          , filteredItems = filteredItems
          , showLoading = showLoading
          , selectedItemIndex = 0
        }
      , getMoreItems text model
      )


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
