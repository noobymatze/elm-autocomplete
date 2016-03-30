module Styling (..) where

import Html exposing (Attribute)
import Html.Attributes exposing (style)
import List


type alias Classes =
  List ( String, Bool )


type alias ClassConfig =
  { menu : Classes
  , list : Classes
  , item : Classes
  , selectedItem : Classes
  , input : Classes
  }


type Components
  = Menu
  | SelectedItem
  | Item
  | List
  | Input


getStyling : Maybe ClassConfig -> Components -> { inlineStyle : Attribute, classes' : Classes }
getStyling maybeClassConfig subcomponent =
  let
    classConfig =
      case maybeClassConfig of
        Just config ->
          config

        Nothing ->
          ClassConfig [] [] [] [] []

    styleWithDefault classList defStyle =
      if List.isEmpty classList then
        style []
      else
        defStyle
  in
    case subcomponent of
      Menu ->
        { inlineStyle = styleWithDefault classConfig.menu menuStyle
        , classes' = classConfig.menu
        }

      SelectedItem ->
        { inlineStyle = styleWithDefault classConfig.selectedItem selectedItemStyle
        , classes' = classConfig.selectedItem
        }

      Item ->
        { inlineStyle = styleWithDefault classConfig.item itemStyle
        , classes' = classConfig.item
        }

      List ->
        { inlineStyle = styleWithDefault classConfig.list listStyle
        , classes' = classConfig.list
        }

      Input ->
        { inlineStyle = styleWithDefault classConfig.input inputStyle
        , classes' = classConfig.input
        }


menuStyle : Attribute
menuStyle =
  style
    [ ( "position", "absolute" )
    , ( "top", "left" )
    , ( "left", "0" )
    , ( "margin-top", "5px" )
    , ( "background", "white" )
    , ( "color", "black" )
    , ( "border", "1px solid #DDD" )
    , ( "border-radius", "3px" )
    , ( "box-shadow", "0 0 5px rgba(0,0,0,0.1)" )
    , ( "min-width", "120px" )
    , ( "z-index", "11110" )
    ]


selectedItemStyle : Attribute
selectedItemStyle =
  style
    [ ( "background", "#3366FF" )
    , ( "color", "white" )
    , ( "display", "block" )
    , ( "padding", "5px 10px" )
    , ( "border-bottom", "1px solid #DDD" )
    , ( "cursor", "pointer" )
    ]


listStyle : Attribute
listStyle =
  style
    [ ( "list-style", "none" )
    , ( "padding", "0" )
    , ( "margin", "auto" )
    , ( "max-height", "200px" )
    , ( "overflow-y", "auto" )
    ]


itemStyle : Attribute
itemStyle =
  style
    [ ( "display", "block" )
    , ( "padding", "5px 10px" )
    , ( "border-bottom", "1px solid #DDD" )
    , ( "cursor", "pointer" )
    ]


inputStyle : Attribute
inputStyle =
  style
    [ ( "min-width", "120px" )
    , ( "color", "black" )
    , ( "position", "relative" )
    , ( "display", "block" )
    , ( "padding", "0.8em" )
    , ( "border", "none" )
    , ( "font-size", "14px" )
    , ( "font-weight", "bold" )
    ]
