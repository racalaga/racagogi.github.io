module CommentId exposing (CommentId, decoder, toString)

import Json.Decode as Decode exposing (Decoder)


type CommentId
    = CommentId Int


decoder : Decoder CommentId
decoder =
    Decode.map CommentId Decode.int


toString : CommentId -> String
toString (CommentId id) =
    String.fromInt id
