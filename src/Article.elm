module Article exposing (Article, Full, Preview, author, body, favorite, favoriteButton, fetch, fromPreview, fullDecoder, mapAuthor, metadata, previewDecoder, slug, unfavorite, unfavoriteButton)

import Api exposing (Cred)
import Api.Endpoint as Endpoint
import Article.Body as Body exposing (Body)
import Article.Slug as Slug exposing (Slug)
import Author exposing (Author)
import Html exposing (Attribute, Html, i)
import Html.Attributes exposing (class)
import Html.Events exposing (stopPropagationOn)
import Http
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, hardcoded, required)
import Time


type Article a
    = Article Internals a


type alias Metadata =
    { description : String
    , title : String
    , tags : List String
    , createdAt : Time.Posix
    , favorited : Bool
    , favoritesCount : Int
    }


type alias Internals =
    { slug : Slug
    , author : Author
    , metadata : Metadata
    }


type Preview
    = Preview


type Full
    = Full Body


author : Article a -> Author
author (Article internals _) =
    internals.author


metadata : Article a -> Metadata
metadata (Article internals _) =
    internals.metadata


slug : Article a -> Slug
slug (Article internals _) =
    internals.slug


body : Article Full -> Body
body (Article _ (Full extraInfo)) =
    extraInfo


mapAuthor : (Author -> Author) -> Article a -> Article a
mapAuthor transform (Article info extras) =
    Article { info | author = transform info.author } extras


fromPreview : Body -> Article Preview -> Article Full
fromPreview newBody (Article info Preview) =
    Article info (Full newBody)


previewDecoder : Maybe Cred -> Decoder (Article Preview)
previewDecoder maybeCred =
    Decode.succeed Article
        |> custom (internalsDecoder maybeCred)
        |> hardcoded Preview


fullDecoder : Maybe Cred -> Decoder (Article Full)
fullDecoder maybeCred =
    Decode.succeed Article
        |> custom (internalsDecoder maybeCred)
        |> required "body" (Decode.map Full Body.decoder)


internalsDecoder : Maybe Cred -> Decoder Internals
internalsDecoder maybeCred =
    Decode.succeed Internals
        |> required "slug" Slug.decoder
        |> required "author" (Author.decoder maybeCred)
        |> custom metadataDecoder


metadataDecoder : Decoder Metadata
metadataDecoder =
    Decode.succeed Metadata
        |> required "description" (Decode.map (Maybe.withDefault "") (Decode.nullable Decode.string))
        |> required "title" Decode.string
        |> required "tagList" (Decode.list Decode.string)
        |> required "createdAt" Iso8601.decoder
        |> required "favorited" Decode.bool
        |> required "favoritesCount" Decode.int


fetch : Maybe Cred -> Slug -> Http.Request (Article Full)
fetch maybeCred articleSlug =
    Decode.field "article" (fullDecoder maybeCred)
        |> Api.get (Endpoint.article articleSlug) maybeCred


favorite : Slug -> Cred -> Http.Request (Article Preview)
favorite articleSlug cred =
    Api.post (Endpoint.favorite articleSlug) (Just cred) Http.emptyBody (faveDecoder cred)


unfavorite : Slug -> Cred -> Http.Request (Article Preview)
unfavorite articleSlug cred =
    Api.delete (Endpoint.favorite articleSlug) cred Http.emptyBody (faveDecoder cred)


faveDecoder : Cred -> Decoder (Article Preview)
faveDecoder cred =
    Decode.field "article" (previewDecoder (Just cred))


favoriteButton :
    Cred
    -> msg
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
favoriteButton _ msg attrs kids =
    toggleFavoriteButton "btn btn-sm btn-outline-primary" msg attrs kids


unfavoriteButton :
    Cred
    -> msg
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
unfavoriteButton _ msg attrs kids =
    toggleFavoriteButton "btn btn-sm btn-primary" msg attrs kids


toggleFavoriteButton :
    String
    -> msg
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
toggleFavoriteButton classStr msg attrs kids =
    Html.button
        (class classStr :: onClickStopPropagation msg :: attrs)
        (i [ class "ion-heart" ] [] :: kids)


onClickStopPropagation : msg -> Attribute msg
onClickStopPropagation msg =
    stopPropagationOn "click"
        (Decode.succeed ( msg, True ))
