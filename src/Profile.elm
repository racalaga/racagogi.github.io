module Profile exposing (Profile, avatar, bio, decoder)

import Avatar exposing (Avatar)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)


type Profile
    = Profile Internals


type alias Internals =
    { bio : Maybe String
    , avatar : Avatar
    }


bio : Profile -> Maybe String
bio (Profile info) =
    info.bio


avatar : Profile -> Avatar
avatar (Profile info) =
    info.avatar


decoder : Decoder Profile
decoder =
    Decode.succeed Internals
        |> required "bio" (Decode.nullable Decode.string)
        |> required "image" Avatar.decoder
        |> Decode.map Profile
