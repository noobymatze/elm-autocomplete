module Main (..) where

import Autocomplete.Simple exposing (Item, ClassListConfig, initWithClasses, initItem, update, view)
import StartApp.Simple
import Html


testData : List Item
testData =
  [ initItem "0" "eggs"
  , initItem "1" "milk"
  , initItem "2" "butter"
  , initItem "3" "bread"
  ]


initExampleClassListConfig : ClassListConfig
initExampleClassListConfig =
  { menu = [ ( "autocomplete-menu-default", True ) ]
  , item = [ ( "autocomplete-item-default", True ) ]
  , selectedItem = [ ( "autocomplete-selected-item-default", True ) ]
  , list = [ ( "autocomplete-list-default", True ) ]
  , input = [ ( "autocomplete-input-default", True ) ]
  }


main : Signal Html.Html
main =
  StartApp.Simple.start
    { model = initWithClasses testData initExampleClassListConfig
    , update = update
    , view = view
    }
