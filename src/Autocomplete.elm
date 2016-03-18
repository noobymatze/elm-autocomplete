module Autocomplete (Autocomplete, Item, ClassListConfig, Classes, init, initWithClasses, initItem, update, view) where

{-| A customizable autocomplete component.

The autocomplete consists of a menu, a list, list items, and an input.
All of the aforementioned are styleable via css classes.

The currently selected item is preserved.

Selection is modified by keyboard input, mouse clicks,
and is also styled via css classes.

# Definition
@docs Autocomplete, Item, ClassListConfig, Classes

# Creating an Autocomplete
@docs init, initWithClasses, initItem

# Update
@docs update

# Views
@docs view

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (..)
import String exposing (..)
import Json.Decode as Json


{-| The Autocomplete model.
    It assumes filtering is based upon strings.
-}
type alias Autocomplete =
  { value : String
  , items : List Item
  , filteredItems : List Item
  , filterFn : Item -> String -> Bool
  , selectedItemIndex : Int
  , classes : ClassListConfig
  }


{-| A collection of class names attributed to each piece of the component.
-}
type alias ClassListConfig =
  { menu : Classes
  , list : Classes
  , item : Classes
  , selectedItem : Classes
  , input : Classes
  }


{-| Alias for the argument to an elm-html classList
-}
type alias Classes =
  List ( String, Bool )


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


{-| Creates an Autocomplete from a list of items with a default `String.contains` filter
-}
init : List Item -> Autocomplete
init items =
  { value = ""
  , items = items
  , filteredItems = items
  , filterFn = (\item value -> String.contains value item.text)
  , selectedItemIndex = 0
  , classes = ClassListConfig [] [] [] [] []
  }


{-| Creates an Autocomplete with custom class names
-}
initWithClasses : List Item -> ClassListConfig -> Autocomplete
initWithClasses items classListConfig =
  let
    model =
      init items
  in
    { model | classes = classListConfig }


{-| Creates an Autocomplete Item
-}
initItem : ID -> Text -> Item
initItem id text =
  { key = id
  , text = text
  }


type Action
  = SetValue String
  | Complete


{-| The quintessential Elm Architecture reducer.
-}
update : Action -> Autocomplete -> Autocomplete
update action model =
  case action of
    SetValue value ->
      if value == "" then
        { model
          | value = value
          , filteredItems = model.items
        }
      else
        { model
          | value = value
          , filteredItems = List.filter (\item -> model.filterFn item value) model.items
        }

    Complete ->
      let
        firstItem =
          List.head model.filteredItems
      in
        case firstItem of
          Just item ->
            { model | value = item.text }

          Nothing ->
            model


viewInput : Address Action -> Autocomplete -> Html
viewInput address model =
  input
    [ type' "text"
    , on "input" targetValue (Signal.message address << SetValue)
    , onTab address Complete
    , value model.value
    , classList model.classes.input
    ]
    []


{-| The full Autocomplete view, with menu and input.
    Needs a Signal.Address and Autocomplete (typical of the Elm Architecture).
-}
view : Address Action -> Autocomplete -> Html
view address model =
  div
    [ id "autocomplete" ]
    [ viewInput address model
    , viewMenu model
    ]


viewItem : Autocomplete -> Item -> Html
viewItem model item =
  li
    [ id item.key
    , classList model.classes.item
    ]
    [ text item.text ]


viewSelectedItem : Autocomplete -> Item -> Html
viewSelectedItem model item =
  li
    [ id item.key
    , classList (List.append model.classes.item model.classes.selectedItem)
    ]
    [ text item.text ]


viewMenu : Autocomplete -> Html
viewMenu model =
  let
    getItemView index item =
      if index == model.selectedItemIndex then
        viewSelectedItem model item
      else
        viewItem model item
  in
    div
      [ classList model.classes.menu ]
      [ ul
          [ classList model.classes.list ]
          (List.indexedMap getItemView model.filteredItems)
      ]


onTab : Signal.Address a -> a -> Attribute
onTab address value =
  onWithOptions
    "keydown"
    { defaultOptions | preventDefault = True }
    (Json.customDecoder keyCode is9)
    (\_ -> Signal.message address value)


is9 : Int -> Result String ()
is9 code =
  if code == 9 then
    Ok ()
  else
    Err "not the right key code"
