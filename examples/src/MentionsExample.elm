module Main (..) where

import StartApp.Simple
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict exposing (Dict)
import Json.Decode as Json
import String
import AtMention exposing (AtMention)
import Autocomplete.Simple as Autocomplete


type alias Model =
  { mentions : Dict Position AtMention
  , value : String
  , currentMentionPos : Maybe Position
  }


init : Model
init =
  { mentions = Dict.empty
  , value = ""
  , currentMentionPos = Nothing
  }


type alias Position =
  Int


type Action
  = NoOp
  | AtMention AtMention.Action Position AtMention
  | SetValue String
  | ToggleMenu Bool


update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

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
          { model
            | mentions = Dict.insert pos updatedMention model.mentions
            , value = newValue
          }
        else
          { model
            | mentions = Dict.insert pos updatedMention model.mentions
          }

    SetValue value ->
      let
        ( updatedMentions, updatedMentionPos ) = updateMentionValue model value
      in
        { model
          | value = value
          , mentions = updatedMentions
          , currentMentionPos = updatedMentionPos
        }
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
            { model |
                mentions  = updatedMentions mentionPos model.mentions
            }
          Nothing ->
            model

updateMentionValue : Model -> String -> ( Dict Position AtMention, Maybe Position )
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
        ( Dict.insert position mention model.mentions, Just pos )
    in
      case model.currentMentionPos of
        Just pos ->
          if String.endsWith " " value then
            ( Dict.remove pos model.mentions, Nothing )
          else
            (AtMention.setValue (getNewMentionValue pos)) (getMention pos model.mentions)
              |> (\mention -> updateMentions mention pos )

        Nothing ->
          if String.endsWith "@" value then
            ( Dict.insert position AtMention.init model.mentions, Just position )
          else
            ( model.mentions, model.currentMentionPos )

getMention : Position -> Dict Position AtMention -> AtMention
getMention pos mentions =
  Maybe.withDefault AtMention.init (Dict.get pos mentions)

view : Signal.Address Action -> Model -> Html
view address model =
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
    div
      [ style [ ("display", "flex"), ("flex-direction", "column"), ("justify-content", "center") ]
      ,  on "keydown" keyCode (\code -> Signal.message address <| (toggleMenu code))
      ]
      [ h1 [] [ text "Mentions Example" ]
        , textarea
          [ on "input" targetValue (Signal.message address << SetValue)
          , onWithOptions "keydown" options dec (\code -> Signal.message address <| (navigate code))
          , value model.value
          , class "editor"

          ]
          []
      , case model.currentMentionPos of
          Just pos ->
            let
              mention =
                Maybe.withDefault AtMention.init (Dict.get pos model.mentions)
            in
              AtMention.view (Signal.forwardTo address (\act -> AtMention act pos mention)) mention

          Nothing ->
            div [] []
      ]


main : Signal Html.Html
main =
  StartApp.Simple.start
    { model = init
    , update = update
    , view = view
    }
