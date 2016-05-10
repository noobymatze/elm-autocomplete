module Autocomplete.Msg exposing (Msg (..))

{-| A description of a state change
-}
type Msg
  = Complete
  | ChangeSelection Int
  | ShowMenu Bool
  | UpdateItems (List String)
  | SetValue String
