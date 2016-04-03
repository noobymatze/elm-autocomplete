# Elm Autocomplete

An Autocomplete component in Elm because typing is hard.

Code up a simple, static set of items for Autocomplete:
```elm
main : Signal Html.Html
main =
  StartApp.Simple.start
    { model = Autocomplete.init [ "elm", "makes", "coding", "life", "easy" ]
    , update = Autocomplete.update
    , view = Autocomplete.view
    }
```

Or make you a fetching, dynamic Autocomplete for great good:

![demo](https://cloud.githubusercontent.com/assets/3099999/14199207/2e19fd3a-f797-11e5-9408-db13e78a4406.gif)

```elm
fetchMoreItems : String -> Task Effects.Never (List String)
fetchMoreItems url =
  Http.url url []
    |> Http.getString
    |> Task.toMaybe
    |> Task.map responseToItems


responseToItems : Maybe String -> List String
responseToItems maybeString =
  case maybeString of
    Just string ->
      String.lines string

    Nothing ->
      []


getItemsTask : String -> Int -> Task Effects.Never (List String)
getItemsTask value index =
  fetchMoreItems "https://raw.githubusercontent.com/first20hours/google-10000-english/master/20k.txt"


app =
  let
    config =
      Autocomplete.Config.defaultConfig
        |> Autocomplete.Config.setLoadingDisplay (img [ src "assets/loading.svg" ] [])
  in
    StartApp.start
      { init = Autocomplete.init [] getItemsTask
      , update = Autocomplete.update
      , view = Autocomplete.view
      , inputs = []
      }


main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
```

### These and More Examples
See the `example` directory
