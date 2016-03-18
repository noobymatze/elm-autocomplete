module Main (..) where

import Autocomplete exposing (initWithClasses, initItem, update, view)
import StartApp.Simple


testData =
  [ initItem "0" "eggs"
  , initItem "1" "milk"
  , initItem "2" "butter"
  , initItem "3" "bread"
  ]


initExampleClassListConfig =
  { menu = [ ( "autocomplete-menu-default", True ) ]
  , item = [ ( "autocomplete-item-default", True ) ]
  , selectedItem = [ ( "autocomplete-selected-item-default", True ) ]
  , list = [ ( "autocomplete-list-default", True ) ]
  , input = [ ( "autocomplete-input-default", True ) ]
  }


main =
  StartApp.Simple.start
    { model = initWithClasses initExampleClassListConfig testData
    , update = update
    , view = view
    }
