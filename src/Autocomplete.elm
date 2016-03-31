module Autocomplete (Autocomplete, Item, ClassListConfig, ClassList, init, initWithClasses, initItem, initItemCustomHtml, update, view) where

{-| A customizable autocomplete component.

The autocomplete consists of a menu, a list, list items, and an input.
All of the aforementioned are styleable via css classes.

The currently selected item is preserved.

Selection is modified by keyboard input, mouse clicks,
and is also styled via css classes.

# Definition
@docs Autocomplete, Item, ClassListConfig, ClassList

# Creating an Autocomplete
@docs init, initWithClasses, initItem, initItemCustomHtml

# Update
@docs update

# Views
@docs view

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
  , preview : String
  , items : List Item
  , maxListSize : Int
  , filteredItems : List Item
  , filterFn : Item -> String -> Bool
  , compareFn : Item -> Item -> Order
  , getItemsTask : GetItemsTask
  , selectedItemIndex : Index
  , classes : Maybe ClassListConfig
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


{-| Creates an Autocomplete from a list of items with a default `String.contains` filter
-}
init : List Item -> Int -> GetItemsTask -> ( Autocomplete, Effects Action )
init items maxListSize getItemsTask =
  ( { value = ""
    , preview = ""
    , items = items
    , maxListSize = maxListSize
    , filteredItems = items
    , filterFn = (\item value -> String.contains value item.text)
    , compareFn = normalComparison
    , getItemsTask = getItemsTask
    , selectedItemIndex = 0
    , classes = Nothing
    }
  , Effects.none
  )


{-| Creates an Autocomplete with custom class names
-}
initWithClasses : List Item -> Int -> GetItemsTask -> ClassListConfig -> ( Autocomplete, Effects Action )
initWithClasses items maxListSize getItemsTask classListConfig =
  ( { value = ""
    , preview = ""
    , items = items
    , filteredItems = items
    , maxListSize = maxListSize
    , filterFn = (\item value -> String.contains value item.text)
    , compareFn = normalComparison
    , getItemsTask = getItemsTask
    , selectedItemIndex = 0
    , classes = Just classListConfig
    }
  , Effects.none
  )


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


{-| The quintessential Elm Architecture reducer.
-}
update : Action -> Autocomplete -> ( Autocomplete, Effects Action )
update action model =
  case action of
    NoOp ->
      ( model, Effects.none )

    SetValue value ->
      if value == "" then
        ( { model
            | value = value
            , filteredItems =
                model.items
                  |> List.sortWith model.compareFn
          }
        , Effects.none
        )
      else
        ( { model
            | value = value
            , filteredItems =
                List.filter (\item -> model.filterFn item value) model.items
                  |> List.sortWith model.compareFn
          }
        , getMoreItems value model
        )

    UpdateItems items ->
      ( { model
          | items = items
          , filteredItems = List.filter (\item -> model.filterFn item model.value) model.items
        }
      , Effects.none
      )

    Complete ->
      case (getSelectedItem model) of
        Just item ->
          ( { model | value = item.text }, Effects.none )

        Nothing ->
          ( model, Effects.none )

    ChangeSelection newIndex ->
      let
        boundedNewIndex =
          Basics.max newIndex 0
            |> Basics.min ((List.length model.filteredItems) - 1)

        selectedItemText =
          case (getSelectedItem model) of
            Just item ->
              item.text

            Nothing ->
              model.value
      in
        ( { model | selectedItemIndex = boundedNewIndex, preview = selectedItemText }, Effects.none )


{-| The full Autocomplete view, with menu and input.
    Needs a Signal.Address and Autocomplete (typical of the Elm Architecture).
-}
view : Signal.Address Action -> Autocomplete -> Html
view address model =
  div
    [ id "autocomplete" ]
    [ viewInput address model
    , viewMenu address model
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
      , value model.value
      , classList (getStyling model.classes Styling.Input).classes'
      , (getStyling model.classes Styling.Input).inlineStyle
      , autocomplete True
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
    [ item.html ]


viewSelectedItem : Signal.Address Action -> Autocomplete -> Item -> Html
viewSelectedItem address model item =
  li
    [ id item.key
    , classList (getStyling model.classes Styling.SelectedItem).classes'
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


getSelectedItem : Autocomplete -> Maybe Item
getSelectedItem model =
  List.drop model.selectedItemIndex model.filteredItems
    |> List.head


getMoreItems : String -> Autocomplete -> Effects Action
getMoreItems value model =
  model.getItemsTask value model.selectedItemIndex
    |> Task.map UpdateItems
    |> Effects.task
