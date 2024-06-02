module NonEmptyList exposing (NonEmptyList)


type alias NonEmptyList a =
    { head : a
    , tail : List a
    }
