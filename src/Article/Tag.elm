module Article.Tag exposing (Tag, list, toString)

import Api
import Api.Endpoint as Endpoint
import Http
import Json.Decode as Decode exposing (Decoder)


type Tag
    = Tag String


toString : Tag -> String
toString (Tag slug) =
    slug


list : Http.Request (List Tag)
list =
    Decode.field "tags" (Decode.list decoder)
        |> Api.get Endpoint.tags Nothing


decoder : Decoder Tag
decoder =
    Decode.map Tag Decode.string
