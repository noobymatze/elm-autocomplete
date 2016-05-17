# Elm Autocomplete

An Autocomplete component in Elm because typing is hard.

Here's a simple example.

```elm
main =
  let
    updateAutocomplete msg autocomplete =
      let
        ( updatedAutocomplete, status ) = Autocomplete.update msg autocomplete
        -- status communicates extra information the parent on every update
        -- e.g. when the selection changes, the value changes, or the user has triggered a completion
      in
        updatedAutocomplete
  in
    Html.beginnerProgram
      { model = Autocomplete.init [ "elm", "makes", "coding", "life", "easy" ]
      , update = updateAutocomplete
      , view = Autocomplete.view
      }
```

## Demos

Github mention style defaults

![simple-autocomplete-elm](https://cloud.githubusercontent.com/assets/3099999/15311173/ec6c0bfa-1bac-11e6-85e1-b19f30bcdcfe.gif)

Custom styles via CSS classnames. Maybe insert custom HTML for items

![typeahead-elm](https://cloud.githubusercontent.com/assets/3099999/15311152/aacb0746-1bac-11e6-9e2f-b4c30cc90345.gif)

Control autocomplete inside a textarea or contenteditable

![mentions](http://g.recordit.co/NIqgZm5zs6.gif)


## Try it out

Find these examples and more in the `./examples` folder.
