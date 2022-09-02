module Author exposing (Author(..), FollowedAuthor, UnfollowedAuthor, decoder, fetch, follow, followButton, profile, requestFollow, requestUnfollow, unfollow, unfollowButton, username, view)

import Api exposing (Cred)
import Api.Endpoint as Endpoint
import Html exposing (Html, a, i, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, optional, required)
import Profile exposing (Profile)
import Route
import Username exposing (Username)


type Author
    = IsFollowing FollowedAuthor
    | IsNotFollowing UnfollowedAuthor
    | IsViewer Cred Profile


type FollowedAuthor
    = FollowedAuthor Username Profile


type UnfollowedAuthor
    = UnfollowedAuthor Username Profile


username : Author -> Username
username author =
    case author of
        IsViewer cred _ ->
            Api.username cred

        IsFollowing (FollowedAuthor val _) ->
            val

        IsNotFollowing (UnfollowedAuthor val _) ->
            val


profile : Author -> Profile
profile author =
    case author of
        IsViewer _ val ->
            val

        IsFollowing (FollowedAuthor _ val) ->
            val

        IsNotFollowing (UnfollowedAuthor _ val) ->
            val


fetch : Username -> Maybe Cred -> Http.Request Author
fetch uname maybeCred =
    Decode.field "profile" (decoder maybeCred)
        |> Api.get (Endpoint.profiles uname) maybeCred


follow : UnfollowedAuthor -> FollowedAuthor
follow (UnfollowedAuthor uname prof) =
    FollowedAuthor uname prof


unfollow : FollowedAuthor -> UnfollowedAuthor
unfollow (FollowedAuthor uname prof) =
    UnfollowedAuthor uname prof


requestFollow : UnfollowedAuthor -> Cred -> Http.Request Author
requestFollow (UnfollowedAuthor uname _) cred =
    Api.post (Endpoint.follow uname) (Just cred) Http.emptyBody (followDecoder cred)


requestUnfollow : FollowedAuthor -> Cred -> Http.Request Author
requestUnfollow (FollowedAuthor uname _) cred =
    Api.delete (Endpoint.follow uname)
        cred
        Http.emptyBody
        (followDecoder cred)


followDecoder : Cred -> Decoder Author
followDecoder cred =
    Decode.field "profile" (decoder (Just cred))


followButton :
    (Cred -> UnfollowedAuthor -> msg)
    -> Cred
    -> UnfollowedAuthor
    -> Html msg
followButton toMsg cred ((UnfollowedAuthor uname _) as author) =
    toggleFollowButton "Follow"
        [ "btn-outline-secondary" ]
        (toMsg cred author)
        uname


unfollowButton :
    (Cred -> FollowedAuthor -> msg)
    -> Cred
    -> FollowedAuthor
    -> Html msg
unfollowButton toMsg cred ((FollowedAuthor uname _) as author) =
    toggleFollowButton "Unfollow"
        [ "btn-secondary" ]
        (toMsg cred author)
        uname


toggleFollowButton : String -> List String -> msg -> Username -> Html msg
toggleFollowButton txt extraClasses msgWhenClicked uname =
    let
        classStr =
            "btn btn-sm " ++ String.join " " extraClasses ++ " action-btn"

        caption =
            "\u{00A0}" ++ txt ++ " " ++ Username.toString uname
    in
    Html.button [ class classStr, onClick msgWhenClicked ]
        [ i [ class "ion-plus-round" ] []
        , text caption
        ]


decoder : Maybe Cred -> Decoder Author
decoder maybeCred =
    Decode.succeed Tuple.pair
        |> custom Profile.decoder
        |> required "username" Username.decoder
        |> Decode.andThen (decodeFromPair maybeCred)


decodeFromPair : Maybe Cred -> ( Profile, Username ) -> Decoder Author
decodeFromPair maybeCred ( prof, uname ) =
    case maybeCred of
        Nothing ->
            Decode.succeed (IsNotFollowing (UnfollowedAuthor uname prof))

        Just cred ->
            if uname == Api.username cred then
                Decode.succeed (IsViewer cred prof)

            else
                nonViewerDecoder prof uname


nonViewerDecoder : Profile -> Username -> Decoder Author
nonViewerDecoder prof uname =
    Decode.succeed (authorFromFollowing prof uname)
        |> optional "following" Decode.bool False


authorFromFollowing : Profile -> Username -> Bool -> Author
authorFromFollowing prof uname isFollowing =
    if isFollowing then
        IsFollowing (FollowedAuthor uname prof)

    else
        IsNotFollowing (UnfollowedAuthor uname prof)


view : Username -> Html msg
view uname =
    a [ class "author", Route.href (Route.Profile uname) ]
        [ Username.toHtml uname ]
