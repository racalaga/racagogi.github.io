module Viewer exposing (Viewer, avatar, cred, decoder, minPasswordChars, store, username)

import Api exposing (Cred)
import Avatar exposing (Avatar)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom)
import Username exposing (Username)


type Viewer
    = Viewer Avatar Cred


cred : Viewer -> Cred
cred (Viewer _ val) =
    val


username : Viewer -> Username
username (Viewer _ val) =
    Api.username val


avatar : Viewer -> Avatar
avatar (Viewer val _) =
    val


minPasswordChars : Int
minPasswordChars =
    6


decoder : Decoder (Cred -> Viewer)
decoder =
    Decode.succeed Viewer
        |> custom (Decode.field "image" Avatar.decoder)


store : Viewer -> Cmd msg
store (Viewer avatarVal credVal) =
    Api.storeCredWith
        credVal
        avatarVal
