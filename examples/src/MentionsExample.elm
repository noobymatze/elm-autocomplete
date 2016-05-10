module Main (..) where

import StartApp
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict exposing (Dict)
import Json.Decode as Json
import Effects exposing (Never, Effects)
import String
import Task
import AtMention exposing (AtMention)
import Autocomplete.Simple as Autocomplete

type alias Model =
  { mentions : Dict Position AtMention
  , value : String
  , currentMentionPos : Maybe Position
  , caretPos: CaretPosition
  }

type alias CaretPosition =
  { top : Int
  , left : Int
  }

init : (Model, Effects Action)
init =
  ({ mentions = Dict.empty
  , value = ""
  , currentMentionPos = Nothing
  , caretPos = { top = 0, left = 0 }
  }, Effects.none)


type alias Position =
  Int


type Action
  = NoOp
  | AtMention AtMention.Action Position AtMention
  | SetValue String
  | ToggleMenu Bool
  | UpdateCaretPosition CaretPosition

update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    NoOp ->
      ( model, Effects.none)

    AtMention act pos mention ->
      let
        ( updatedMention, completed ) =
          AtMention.update act mention

        currentMentionLength =
          AtMention.getValue mention
            |> String.length

        startToMentionSlice =
          String.slice 0 pos model.value

        completedMentionValue =
          AtMention.getValue updatedMention

        mentionStartToEndSlice =
          String.slice (pos + currentMentionLength)  (String.length model.value) model.value

        newValue =
          startToMentionSlice ++ completedMentionValue ++ mentionStartToEndSlice
      in
        if completed then
          ({ model
            | mentions = Dict.insert pos updatedMention model.mentions
            , value = newValue
          }, Effects.none)
        else
          ({ model
            | mentions = Dict.insert pos updatedMention model.mentions
          }, Effects.none)

    SetValue value ->
       ( updateMentionValue model value, Effects.none )


    ToggleMenu bool ->
      let
          updatedMention pos mentions =
            getMention pos mentions
              |> AtMention.showMenu bool
          updatedMentions pos mentions =
              Dict.insert pos (updatedMention pos mentions) mentions
      in
        case model.currentMentionPos of
          Just mentionPos ->
            ({ model |
                mentions  = updatedMentions mentionPos model.mentions
            }, Effects.none )
          Nothing ->
            ( model, Effects.none )

    UpdateCaretPosition caretPos ->
      ( { model | caretPos = caretPos }, Effects.none )

updateMentionValue : Model -> String -> Model
updateMentionValue model value =
    let
      getMentionLength mention =
        AtMention.getValue mention
          |> String.length

      getNewMentionValue pos =
        String.slice pos (pos + (getMentionLength <| getMention pos model.mentions) + 1) value

      position =
        Maybe.withDefault (String.length value) model.currentMentionPos

      updateMentions mention pos =
        { model
          | value = value
          , mentions = Dict.insert position mention model.mentions
          , currentMentionPos = Just pos
        }
    in
      case model.currentMentionPos of
        Just pos ->
          if String.endsWith " " value then
            { model
              | value = value
              , mentions = Dict.remove pos model.mentions
              , currentMentionPos = Nothing
            }
          else
            (AtMention.setValue (getNewMentionValue pos)) (getMention pos model.mentions)
              |> (\mention -> updateMentions mention pos )

        Nothing ->
          if String.endsWith "@" value then
            { model
              | value = value
              , mentions = Dict.insert position AtMention.init model.mentions
              , currentMentionPos = Just position
            }
          else
            { model
              | value = value
              , mentions = model.mentions
              , currentMentionPos = model.currentMentionPos
            }

getMention : Position -> Dict Position AtMention -> AtMention
getMention pos mentions =
  Maybe.withDefault AtMention.init (Dict.get pos mentions)

view : Signal.Address Action -> Model -> Html
view address model =
    div []
          [ viewEditor address model
          , case model.currentMentionPos of
              Just pos ->
                let
                  mention =
                    Maybe.withDefault AtMention.init (Dict.get pos model.mentions)
                in
                  div [ style [ ("top", toString model.caretPos.top ++ "px" ), ("left", toString model.caretPos.left ++ "px"), ("position", "absolute") ] ]
                   [ AtMention.view (Signal.forwardTo address (\act -> AtMention act pos mention)) mention ]

              Nothing ->
                div [] []
            ]

viewEditor : Signal.Address Action -> Model -> Html
viewEditor address model =
  let
    options =
      { preventDefault = True, stopPropagation = False }

    dec =
      (Json.customDecoder
        keyCode
        (\k ->
          if List.member k [ 38, 40, 9, 13 ] then
            Ok k
          else
            Err "not handling that key"
        )
      )

    navigateMenu code pos mention =
      case code of
        38 ->
          AtMention (AtMention.NavigateMenu Autocomplete.Previous) pos mention

        40 ->
          AtMention (AtMention.NavigateMenu Autocomplete.Next) pos mention

        _ ->
          AtMention (AtMention.NavigateMenu Autocomplete.Select) pos mention

    navigate code =
      case model.currentMentionPos of
        Just pos ->
          case Dict.get pos model.mentions of
            Just mention ->
              navigateMenu code pos mention

            Nothing ->
              NoOp

        Nothing ->
          NoOp

    toggleMenu code =
      case code of
        27 ->
          ToggleMenu  False
        _ ->
          NoOp
  in
    div [ on "keydown" keyCode (\code -> Signal.message address <| (toggleMenu code)) ]
          [ textarea
            [ on "input" targetValue (Signal.message address << SetValue)
            , onWithOptions "keydown" options dec (\code -> Signal.message address <| (navigate code))
            , value model.value
            , class "editor"
            ]
            []
        ]

app : StartApp.App Model
app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = [ Signal.map UpdateCaretPosition caretPosition]
    }

main : Signal Html
main =
  app.html

port caretPosition : Signal CaretPosition

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
