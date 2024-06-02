module Util exposing (stripLast)


stripLast : List a -> List a
stripLast list =
    List.take (List.length list - 1) list
