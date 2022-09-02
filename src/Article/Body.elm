module Article.Body exposing (Body, MarkdownString, decoder, toHtml, toMarkdownString)

import Html exposing (Attribute, Html)
import Json.Decode as Decode exposing (Decoder)
import Markdown


type Body
    = Body MarkdownString


type alias MarkdownString =
    String


toHtml : Body -> List (Attribute msg) -> Html msg
toHtml (Body markdown) attributes =
    Markdown.toHtml attributes markdown


toMarkdownString : Body -> MarkdownString
toMarkdownString (Body markdown) =
    markdown


decoder : Decoder Body
decoder =
    Decode.map Body Decode.string
